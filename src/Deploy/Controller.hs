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
import Data.Hashable
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

type DeployerId = (String, Int)

nginxfile :: String
nginxfile = "nginx.conf"

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
               , jobEnvs :: String  -- environment vars, separated by newlines
               , jobTarballSize :: Integer
               , jobTarballName :: String
               , jobTarballWriter :: (S.ByteString -> IO ()) -> IO ()  -- takes in deployerPut function and writes tarball to deployer
               }

instance Hashable Job where
  hashWithSalt s job = hashWithSalt s $ jobId job

instance Eq Job where
  a == b = (jobId a) == (jobId b)


type JobHt = H.BasicHashTable Job (MVar Deployer)  -- job id's and their deployers
type DeployerHt = H.BasicHashTable DeployerId (MVar Deployer)  -- deployer ids and deployers

data ControllerState = ControllerState
      { ctrlJobs :: JobHt
      , ctrlDeployers :: DeployerHt }

type Controller = StateT ControllerState IO  -- stores a ControllerState along with every action

removeDeployer :: DeployerId -> MVar () -> MVar Int -> MVar () -> Controller ()
removeDeployer did depMutex jobMutex nginxMutex = do
  deployers <- gets ctrlDeployers
  jobht <- gets ctrlJobs
  md <- liftIO $ atomic depMutex $ stToIO $ do
          md <- HST.lookup deployers did  -- the mvar deployer
          when (isJust md) $ HST.delete deployers did  -- deployer exists, so delete it from ht
          return md
  when (isJust md) $ do
    joblist <- liftIO $ H.toList jobht
    flip mapM joblist $ \(job, mdep) ->  -- re-deploy jobs
      if mdep == (fromJust md) then deployJob job jobMutex nginxMutex
      else return ""
    liftIO $ do
      updateDeployerFile deployers deployerFile depMutex  -- update the file backup
      withMVar (fromJust md) deployerClose  -- close the handle
    return ()

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
          trace ("withDeployer threw an exception") $ return ()
          deployers <- gets ctrlDeployers
          liftIO $ H.delete deployers $ deployerId deployer
          case mretry of
            Nothing -> liftIO $ trace "throwing exception" $ throwIO e
            Just retry -> retry)

crlf :: S.ByteString
crlf = "\r\n"

deployJob :: Job -> MVar Int -> MVar () -> Controller String
deployJob job jobMutex nginxMutex = trace "***deployJob called***" $ do
  md <- chooseDeployer job
  liftIO $ isEmptyMVar md >>= print
  liftIO $ trace "chose deployer" $ return ()
  withDeployer (Just $ deployJob job jobMutex nginxMutex) md $ \deployer -> do  -- to retry, just run deployJob again
    liftIO $ do
      trace ("deploying to: " ++ (show $ deployerId deployer)) $ return ()
      deployerPut deployer $
        "launch" <> crlf
          <> jobCommand job <> crlf
          <> (S8.pack $ show $ jobId job) <> crlf
          <> (S8.pack $ jobEnvs job)
          <> crlf
          <> (S8.pack . show $ jobTarballSize job) <> crlf
      jobTarballWriter job $ deployerPut deployer
      let jobname = S8.unpack $ jobName job
      atomic nginxMutex $
        let (dhost, dport) = deployerId deployer in
        addEntry nginxfile jobname $ DeployInfo (jobId job) dhost dport
      trace "just read tar file" $ return ()
    jobs <- gets ctrlJobs
    liftIO $ do
      H.insert jobs job md
      addJobToFile jobFile job deployer jobMutex
      trace "inserted job into ht" $ return ("Launched new job with ID: " ++ (show $ jobId job))

killJob :: JobId -> String -> MVar Int -> MVar () -> Controller (Either String ())
killJob jid jobname jobMutex nginxMutex = do
  liftIO $ trace "===killJob===" $ return ()
  jobs <- gets ctrlJobs  -- job id's + deployers
  md <- liftIO $ lookupById jobs jid  -- TODO: look up by id
  case md of
    Nothing -> return . Left $ "No job found with id " ++ (show jid)
    Just dmv -> do
      liftIO $ do
        trace "killJob: about to delete job from ht" $ return ()
        atomic jobMutex $ deleteById jobs jid jobMutex
        trace "killJob: just deleted job from ht" $ return ()
        updateJobFile jobs jobFile jobMutex
        trace "killJob: updated job file" $ return ()
      withDeployer Nothing dmv $ \deployer -> liftIO $ do  -- dmv = deployer mvar
        trace "killJob: found job" $ return ()
        atomic nginxMutex $
          let (dhost, dport) = deployerId deployer in
          removeEntry nginxfile jobname $ DeployInfo jid dhost dport
        deployerPut deployer $ "kill" <> crlf <> (S8.pack $ show jid) <> crlf
        ln <- deployerGetLine deployer
        if ln == "NOT FOUND" then
          return . Left $ "Job " ++ (show jid) ++ " not found on deployer"
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

removeJob :: JobId -> String -> MVar Int -> MVar () -> Controller ()
removeJob jobId jobName jobMutex nginxMutex = do
  trace "removeJob called" $ return ()
  jobs <- gets ctrlJobs  -- job id's + deployers
  md <- liftIO $ lookupById jobs jobId  -- deployer
  case md of
    Nothing -> return ()
    Just dmv -> withDeployer Nothing dmv $ \deployer -> liftIO $ do  -- dmv = deployer mvar
      atomic jobMutex $ deleteById jobs jobId jobMutex
      atomic nginxMutex $
        let (dhost, dport) = deployerId deployer in
        removeEntry nginxfile jobName $ DeployInfo jobId dhost dport
      updateJobFile jobs jobFile jobMutex


-- TODO: use mutexes on the files

-- replace the existing file with the contents of the hashtable
updateDeployerFile :: DeployerHt -> FilePath -> MVar () -> IO ()
updateDeployerFile ht filepath mutex = do
  h <- atomic mutex $ openFile filepath WriteMode -- overwrite anything that's currently in the file
  list <- H.toList ht
  _ <- mapM (\(did, mvar) -> atomic mutex $ hPutStrLn h (fst did ++ "," ++ (show $ snd did))) list
  hClose h

--type DeployerHt = H.BasicHashTable DeployerId (MVar Deployer)  -- deployer ids and deployers

-- back up the jobId and its deployer's id to a file
addJobToFile :: FilePath -> Job -> Deployer -> MVar Int -> IO ()
addJobToFile filepath job deployer mutex = trace "adding job to file" $ do
  trace "removed deployer from mvar" $ return ()
  h <- atomic mutex $ openFile filepath AppendMode
  trace "opened file" $ return ()
  hPutStrLn h (stringify job deployer)
  trace "closing file handle" $ hClose h

updateJobFile :: JobHt -> FilePath -> MVar Int -> IO ()
updateJobFile ht filepath mutex = do
  trace "===updateJobFile===" $ return ()
  h <- atomic mutex $ openFile filepath WriteMode -- overwrite anything that's currently in the file
  trace "opened file" $ return ()
  list <- H.toList ht
  trace ("number of jobs now running: " ++ (show $ length list)) $ return ()
  _ <- mapM (addToFile h) list
  trace "addToFile finished" $ return ()
  hClose h
  where addToFile h (job, mdeployer) = do
          trace "===addToFile===" $ return ()
          deployer <- readMVar mdeployer
          trace "addToFile: took deployer from mvar" $ return ()
          hPutStrLn h (stringify job deployer)

stringify job deployer =
  (show $ jobId job) ++ "," ++ (S8.unpack $ jobName job) ++ "," ++
  (S8.unpack $ jobCommand job) ++ "," ++ (show $ jobTarballSize job) ++ "," ++
  jobTarballName job ++ "," ++ (fst $ deployerId deployer) ++ "," ++
  (show $ snd $ deployerId deployer) ++ "\n" ++ jobEnvs job
  -- env vars are separated by newlines

lookupById :: JobHt -> JobId -> IO (Maybe (MVar Deployer))
lookupById ht jid = do
  list <- H.toList ht
  let matches = filter (\(job, md) -> jobId job == jid) list
  case matches of
    [] -> return Nothing
    justmd -> return $ Just $ snd $ head matches

deleteById :: JobHt -> JobId -> MVar Int -> IO ()
deleteById ht jid mutex = do
  list <- H.toList ht
  print "deleteById: converted ht to list"
  let matches = filter (\(job, md) -> jobId job == jid) list
  print ("deleteById: found matches: " ++ (show $ length matches))
  flip mapM_ matches $ \(job, md) -> H.delete ht job
  print "deleteById: finished"
  return ()

