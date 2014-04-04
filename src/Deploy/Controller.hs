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

type JobId = Int

type DeployerId = String

nginxfile :: String
nginxfile = "nginx.conf"

deployerPort :: Int
deployerPort = 9876

data Deployer = Deployer { deployerId :: DeployerId
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

-- TODO: Add environment variables?
data Job = Job { jobId :: JobId
               , jobName :: S.ByteString  -- the name of the app
               , jobCommand :: S.ByteString  -- the shell command to be run
               , jobEnvs :: String  -- environment vars, separated by newlines. change to bytestring?
               , jobTarballSize :: Integer
               , jobTarballWriter :: (S.ByteString -> IO ()) -> IO ()  -- takes in deployerPut function and writes tarball to deployer
               }

data ControllerState = ControllerState
      { ctrlJobs :: H.BasicHashTable JobId (MVar Deployer)  -- job id's and their deployers
      , ctrlDeployers :: H.BasicHashTable DeployerId (MVar Deployer) }  -- deployer ids and deployers

type Controller = StateT ControllerState IO  -- stores a ControllerState along with every action

removeDeployer :: DeployerId -> Controller ()
removeDeployer did = do
  deployers <- gets ctrlDeployers  -- ht of dId's and deployers
  liftIO $ do
    md <- stToIO $ do
            md <- HST.lookup deployers did  -- the mvar deployer
            when (isJust md) $ HST.delete deployers did  -- if deployer exists, delete it from ht
            return md
    when (isJust md) $ withMVar (fromJust md) deployerClose  -- close the handle

addDeployer :: Deployer -> Controller ()
addDeployer deployer = trace "===addDeployer===" $ do
  deployers <- gets ctrlDeployers
  liftIO $ do
    mdeployer <- newMVar deployer
    --isEmpty <- isEmptyMVar mdeployer
    --print ("mvar empty? " ++ show isEmpty)
    H.insert deployers (deployerId deployer) mdeployer
    --list <- H.toList deployers
    --let mdeployer = snd $ list !! 0
    --isEmpty <- liftIO $ isEmptyMVar mdeployer
    --trace ("size of deployers ht: " ++ (show $ length list)) $ return ()
    --trace ("mdeployer empty? " ++ show isEmpty) $ return ()

chooseDeployer :: Job -> Controller (MVar Deployer)
chooseDeployer job = trace "===chooseDeployer===" $ do
  deployers <- liftIO . H.toList =<< gets ctrlDeployers
  trace ("size of deployer ht: " ++ (show $ length deployers)) $ return ()
  let mdeployer = snd $ deployers !! (jobId job `mod` (length deployers))
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
          deployers <- gets ctrlDeployers
          trace "got deployers" $ return ()
          liftIO $ H.delete deployers $ deployerId deployer
          case mretry of
            Nothing -> throwIO e
            Just retry -> retry)

crlf :: S.ByteString
crlf = "\r\n"

deployJob :: Job -> Controller String
deployJob job = trace "deployJob called" $ do
  md <- chooseDeployer job
  liftIO $ isEmptyMVar md >>= print
  liftIO $ trace "chose deployer" $ return ()
  withDeployer (Just $ deployJob job) md $ \deployer -> do  -- to retry, just run deployJob again
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
      --atomic appMutex $ addToFile appFile (show appId) hostname
      H.insert jobs (jobId job) md
      trace "inserted job into ht" $ return ("Launched new job with ID: " ++ (show $ jobId job))

killJob :: JobId -> String -> Controller (Either String ())
killJob jobId jobName = do
  jobs <- gets ctrlJobs  -- job id's + deployers
  md <- liftIO $ H.lookup jobs jobId  -- deployer
  case md of
    Nothing -> return . Left $ "No job found with id " ++ (show jobId)
    Just dmv -> withDeployer Nothing dmv $ \deployer -> liftIO $ do  -- dmv = deployer mvar
      H.delete jobs jobId
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

removeJob :: JobId -> String -> Controller ()
removeJob jobId jobName = do
  jobs <- gets ctrlJobs  -- job id's + deployers
  md <- liftIO $ H.lookup jobs jobId  -- deployer
  case md of
    Nothing -> return ()
    Just dmv -> withDeployer Nothing dmv $ \deployer -> liftIO $ do  -- dmv = deployer mvar
      H.delete jobs jobId
      removeEntry nginxfile jobName $ DeployInfo jobId (deployerId deployer) deployerPort

--removeJob jobId = gets ctrlJobs >>= \jobs -> liftIO $ H.delete jobs jobId

