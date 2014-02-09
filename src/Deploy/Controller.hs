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
import System.IO

type JobId = Int

type DeployerId = String

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

data Job = Job { jobId :: JobId
               , jobName :: S.ByteString
               , jobTarballSize :: Integer
               , jobTarballWriter :: (S.ByteString -> IO ()) -> IO () }

data ControllerState = ControllerState
      { ctrlJobs :: H.BasicHashTable JobId (MVar Deployer)
      , ctrlDeployers :: H.BasicHashTable DeployerId (MVar Deployer) }

type Controller = StateT ControllerState IO

removeDeployer :: DeployerId -> Controller ()
removeDeployer did = do
  deployers <- gets ctrlDeployers
  liftIO $ do
    md <- stToIO $ do
            md <- HST.lookup deployers did
            when (isJust md) $ HST.delete deployers did
            return md
    when (isJust md) $ withMVar (fromJust md) deployerClose

addDeployer :: Deployer -> Controller ()
addDeployer deployer = do
  deployers <- gets ctrlDeployers
  liftIO $ do
    mdeployer <- newMVar deployer
    H.insert deployers (deployerId deployer) mdeployer

chooseDeployer :: Job -> Controller (MVar Deployer)
chooseDeployer job = do
  deployers <- liftIO . H.toList =<< gets ctrlDeployers
  return . snd $ deployers !! (jobId job `mod` (length deployers))

withDeployer :: Maybe (Controller a) -> MVar Deployer -> (Deployer -> Controller a) -> Controller a
withDeployer mretry mdeployer func = do
  bracket (liftIO $ takeMVar mdeployer) (liftIO . putMVar mdeployer) $ \deployer ->
    func deployer `catch`
      (\(e :: IOException) -> do
          deployers <- gets ctrlDeployers
          liftIO $ H.delete deployers $ deployerId deployer
          case mretry of
            Nothing -> throwIO e
            Just retry -> retry)

crlf :: S.ByteString
crlf = "\r\n"

deployJob :: Job -> Controller ()
deployJob job = do
  md <- chooseDeployer job
  withDeployer (Just $ deployJob job) md $ \deployer -> do
    liftIO $ do
      deployerPut deployer $
        "launch" <> crlf
          <> jobName job <> crlf
          <> (S8.pack . show $ jobTarballSize job) <> crlf
      jobTarballWriter job $ deployerPut deployer
    jobs <- gets ctrlJobs
    liftIO $ H.insert jobs (jobId job) md

killJob :: JobId -> Controller (Either String ())
killJob jobId = do
  jobs <- gets ctrlJobs
  md <- liftIO $ H.lookup jobs jobId
  case md of
    Nothing -> return . Left $ "No job found with id " ++ (show jobId)
    Just dmv -> withDeployer Nothing dmv $ \deployer -> liftIO $ do
      H.delete jobs jobId
      deployerPut deployer $ "kill" <> crlf <> (S8.pack $ show jobId) <> crlf
      ln <- deployerGetLine deployer
      if ln == "NOT FOUND" then
        return . Left $ "Job " ++ (show jobId) ++ " not found on deployer"
        else return $ Right ()

deployerStats :: DeployerId -> Controller (Either String S.ByteString)
deployerStats did = do
  deployers <- gets ctrlDeployers
  mdvar <- liftIO $ H.lookup deployers did
  case mdvar of
    Nothing -> return $ Left $ "No deployer with id " ++ (show did)
    Just dvar -> do
      liftIO $ withMVar dvar $ \deployer -> do
        deployerPut deployer $ "statuses" <> crlf
        Right <$> deployerGetLine deployer

removeJob :: JobId -> Controller ()
removeJob jobId = gets ctrlJobs >>= \jobs -> liftIO $ H.delete jobs jobId
