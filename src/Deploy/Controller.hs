{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Deploy.Controller where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST.Safe
import Control.Exception.Peel
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.HashTable.IO as H
import qualified Data.HashTable.ST.Basic as HST
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans.State
import Debug.Trace
import System.IO
import NginxUpdater
import Utils

type JobId = Int

type DeployerId = String

nginxfile :: String
nginxfile = "nginx.conf"

deployerPort :: Int
deployerPort = 9876

jobFile :: FilePath  -- backup of the ctrlJobs hashtable, with key/value pairs separated by commas
jobFile = "jobinfo.txt"

deployerFile :: FilePath  -- list of all the deployer id's
deployerFile = "deployerinfo.txt"

data Deployer = Deployer { deployerId :: DeployerId  -- aka hostname
                         , deployerPut :: S.ByteString -> IO ()
                         , deployerGet :: Int -> IO S.ByteString
                         , deployerGetLine :: IO S.ByteString
                         , deployerClose :: IO () }

deployerFromHandle :: DeployerId -> Handle -> IO Deployer
deployerFromHandle did handle = return $
  Deployer { deployerId = did
           , deployerPut = S.hPut handle
           , deployerGet = S.hGet handle
           , deployerGetLine = S.hGetLine handle
           , deployerClose = hClose handle }

type DeployerStatus = Int

data Job = Job { jobId :: JobId
               , jobName :: S.ByteString  -- the name of the app
               , jobCommand :: S.ByteString  -- the shell command to be run
               , jobEnvs :: String  -- environment vars, separated by newlines. change to bytestring?
               , jobTarballSize :: Integer
               , jobTarballWriter :: (S.ByteString -> IO ()) -> IO ()  -- takes in deployerPut function and writes tarball to deployer
               }

type JobHt = H.BasicHashTable JobId (MVar Deployer)  -- job id's and their deployers
type DeployerHt = H.BasicHashTable DeployerId (MVar Deployer)  -- deployer ids and deployers

data ControllerState = ControllerState
      { ctrlJobs :: JobHt
      , ctrlDeployers :: DeployerHt }

type Controller = StateT ControllerState IO  -- stores a ControllerState along with every action

removeDeployer :: DeployerId -> MVar () -> Controller ()
removeDeployer did mutex = do
  deployers <- gets ctrlDeployers  -- ht of dId's and deployers
  liftIO $ do
    md <- stToIO $ do
            md <- HST.lookup deployers did  -- the mvar deployer
            when (isJust md) $ HST.delete deployers did  -- deployer exists, so delete it from ht
            return md
    when (isJust md) $ do
      withMVar (fromJust md) deployerClose  -- close the handle
      updateDeployerFile deployers deployerFile mutex  -- update the file backup

addDeployer :: Deployer -> MVar () -> Controller ()
addDeployer deployer mutex = trace "===addDeployer===" $ do
  deployers <- gets ctrlDeployers
  liftIO $ do
    mdeployer <- newMVar deployer
    H.insert deployers (deployerId deployer) mdeployer
    trace "about to add deployer to file" $ return ()
    updateDeployerFile deployers deployerFile mutex
    --addDeployerToFile deployerFile (deployerId deployer) mutex

chooseDeployer :: Job -> Controller (MVar Deployer)
chooseDeployer job = trace "===chooseDeployer===" $ do
  deployers <- liftIO . H.toList =<< gets ctrlDeployers
  trace ("number of deployers in ht: " ++ (show $ length deployers)) $ return ()
  let mdeployer = snd $ deployers !! (jobId job `mod` (length deployers))
  --let mdeployer = snd $ deployers !! (jobId job `mod` (length deployers))
  isEmpty <- liftIO $ isEmptyMVar mdeployer
  trace ("mdeployer empty? " ++ show isEmpty) $ return ()
  return . snd $ deployers !! (jobId job `mod` (length deployers))

withDeployer :: Maybe (Controller a) -> MVar Deployer -> (Deployer -> Controller a) -> Controller a
withDeployer mretry mdeployer func = trace "===withdeployer===" $ do
  bracket (liftIO $ takeMVar mdeployer) (liftIO . putMVar mdeployer) $ \deployer -> do
    liftIO $ trace "about to get deployer out of mvar" $ return ()
    -- get the mdeployer out of its mvar, do the stuff below, and then put it back
    func deployer `catch`  -- what happens if the deployer is down or whatever
      (\(e :: IOException) -> do
          trace ("withDeployer threw exception: " ++ show e) $ return ()
          deployers <- gets ctrlDeployers
          liftIO $ H.delete deployers $ deployerId deployer
          case mretry of
            Nothing -> liftIO $ trace "throwing exception" $ throwIO e
            Just retry -> retry)

crlf :: S.ByteString
crlf = "\r\n"

deployJob :: Job -> MVar Int -> Controller String
deployJob job mutex = trace "deployJob called" $ do
  md <- chooseDeployer job
  liftIO $ isEmptyMVar md >>= print
  liftIO $ trace "chose deployer" $ return ()
  withDeployer (Just $ deployJob job mutex) md $ \deployer -> do  -- to retry, just run deployJob again
    liftIO $ do
      trace "about to read tar file" $ return ()
      deployerPut deployer $
        "launch" <> crlf
          <> jobCommand job <> crlf
          <> (S8.pack $ show $ jobId job) <> crlf
          <> (S8.pack $ jobEnvs job)
          <> crlf
          <> (S8.pack . show $ jobTarballSize job) <> crlf
      jobTarballWriter job $ deployerPut deployer
      let jobname = S8.unpack $ jobName job
      addEntry nginxfile jobname $ DeployInfo (jobId job) (deployerId deployer) deployerPort
      trace "just read tar file" $ return ()
    jobs <- gets ctrlJobs
    liftIO $ do
      H.insert jobs (jobId job) md
      addJobToFile jobFile (jobId job) deployer mutex
      trace "inserted job into ht" $ return ("Launched new job with ID: " ++ (show $ jobId job))

killJob :: JobId -> String -> MVar Int -> Controller (Either String ())
killJob jobId jobName mutex = do
  liftIO $ trace "===killJob===" $ return ()
  jobs <- gets ctrlJobs  -- job id's + deployers
  md <- liftIO $ H.lookup jobs jobId  -- deployer
  case md of
    Nothing -> return . Left $ "No job found with id " ++ (show jobId)
    Just dmv -> do
      liftIO $ do
        H.delete jobs jobId
        updateJobFile jobs jobFile mutex
        trace "killJob: updated job file" $ return ()
      withDeployer Nothing dmv $ \deployer -> liftIO $ do  -- dmv = deployer mvar
        trace "killJob: found job" $ return ()
        removeEntry nginxfile jobName $ DeployInfo jobId (deployerId deployer) deployerPort
        deployerPut deployer $ "kill" <> crlf <> (S8.pack $ show jobId) <> crlf
        ln <- deployerGetLine deployer
        if ln == "NOT FOUND" then
          return . Left $ "Job " ++ (show jobId) ++ " not found on deployer"
          else return $ Right ()

deployerStats :: DeployerId -> Controller (Either String S.ByteString)
deployerStats did = do
  deployers <- gets ctrlDeployers
  mdvar <- liftIO $ H.lookup deployers did
  liftIO $ trace "got deployers and mdvar" $ return ()
  case mdvar of
    Nothing -> return $ Left $ "No deployer with id " ++ (show did)
    Just dvar -> do
      liftIO $ withMVar dvar $ \deployer -> do
        deployerPut deployer $ "statuses" <> crlf
        Right <$> deployerGetLine deployer

removeJob :: JobId -> String -> MVar Int -> Controller ()
removeJob jobId jobName mutex = do
  jobs <- gets ctrlJobs  -- job id's + deployers
  md <- liftIO $ H.lookup jobs jobId  -- deployer
  case md of
    Nothing -> return ()
    Just dmv -> withDeployer Nothing dmv $ \deployer -> liftIO $ do  -- dmv = deployer mvar
      H.delete jobs jobId
      removeEntry nginxfile jobName $ DeployInfo jobId (deployerId deployer) deployerPort
      updateJobFile jobs jobFile mutex


-- TODO: use mutexes on the files

-- replace the existing file with the contents of the hashtable
updateDeployerFile :: DeployerHt -> FilePath -> MVar () -> IO ()
updateDeployerFile ht filepath mutex = do
  h <- atomic mutex $ openFile filepath WriteMode -- overwrite anything that's currently in the file
  list <- H.toList ht
  _ <- mapM (\pair -> atomic mutex $ hPutStrLn h $ fst pair) list
  hClose h

-- back up the jobId and its deployer's id to a file
addJobToFile :: FilePath -> JobId -> Deployer -> MVar Int -> IO ()
addJobToFile filepath jobId deployer mutex = trace "adding job to file" $ do
  trace "removed deployer from mvar" $ return ()
  h <- openFile filepath AppendMode
  trace "opened file" $ return ()
  hPutStrLn h $ (show jobId) ++ "," ++ (deployerId deployer)
  trace "closing file handle" $ hClose h

updateJobFile :: H.BasicHashTable JobId (MVar Deployer) -> FilePath -> MVar Int -> IO ()
updateJobFile ht filepath mutex = do
  trace "===updateJobFile===" $ return ()
  h <- atomic mutex $ openFile filepath WriteMode -- overwrite anything that's currently in the file
  trace "opened file" $ return ()
  list <- H.toList ht
  trace ("number of jobs now running: " ++ (show $ length list)) $ return ()
  _ <- mapM (addToFile h) list
  trace "addToFile finished" $ return ()
  hClose h
  where addToFile h (jobId, mdeployer) = do
          trace "===addToFile===" $ return ()
          deployer <- readMVar mdeployer
          trace "addToFile: took deployer from mvar" $ return ()
          atomic mutex $ hPutStrLn h (show jobId ++ "," ++ deployerId deployer)

