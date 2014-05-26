module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.HashTable.IO as H
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.String.Utils
import Debug.Trace
import Network
import System.IO
import Deploy.Controller
import Utils


main :: IO ()
main = bracket (listenOn $ PortNumber 1234) sClose $ \s -> forever $ do
  jobMutex <- newMVar 0  -- for job backup file
  deployerMutex <- newMVar ()  -- for deployer backup file
  nginxMutex <- newMVar ()  -- for nginx config file
  (h, _, _) <- accept s
  jobs <- fillJobsFromFile jobFile jobMutex
  deployers <- fillDeployerFromFile deployerFile deployerMutex
  let cstate = ControllerState jobs deployers
  forkIO $ flip evalStateT cstate $ forever $ do  -- thread to monitor deployers
    deployerht <- gets ctrlDeployers
    jobht <- gets ctrlJobs
    liftIO $ atomic deployerMutex $ flip H.mapM_ deployerht $ \pair -> do -- pair = (did, mdep)
      alive <- checkDeployer deployerht jobht jobMutex nginxMutex pair
      --if alive then trace ("deployer " ++ (show $ fst pair) ++ " alive") $ return ()  -- deployer is fine
      if alive then return ()  -- deployer is fine
      else evalStateT (removeDeployer (fst pair) deployerMutex jobMutex nginxMutex) cstate
    return ()
  forkIO $ do
    _ <- execStateT (handleConnection h jobMutex deployerMutex nginxMutex) cstate
    hClose h
  return ()

handleConnection :: Handle -> MVar Int -> MVar () -> MVar () -> Controller ()
handleConnection chandle jobMutex deployerMutex nginxMutex = foreverOrEOF chandle $ do
    cmd <- liftIO $ trim `fmap` hGetLine chandle
    case cmd of
      "statuses" -> do  -- show statuses of all app deployers in the hashtable
          deployerht <- gets ctrlDeployers  -- ht of dId's and deployers
          liftIO $ do
            deployerList <- H.toList deployerht
            mdeployers <- mapM (readMVar . snd) deployerList  -- list of mvar deployers
            hPutStrLn chandle $ show $ map deployerId mdeployers
      "deployer" -> do  -- show statuses of all apps of a deployer
          -- format:
          -- hostname
          -- port num
          hostname <- liftIO $ trim `fmap` hGetLine chandle 
          liftIO $ trace (show hostname) $ return ()
          deployerPort <- liftIO $ (read . trim) `fmap` hGetLine chandle
          estats <- deployerStats (hostname, deployerPort)
          liftIO $ trace (show estats) $ return ()
          liftIO $ case estats of
            Left msg -> hPutStrLn chandle msg
            Right msg -> hPutStrLn chandle $ S8.unpack msg
      "run" -> do  -- run an app
          -- format:
          -- app name
          -- shell command to be run
          -- var1=val1
          -- var2=val2
          -- ...
          --
          -- size of tar file
          -- tar file path
          appname <- liftIO $ (S8.pack . trim) `fmap` hGetLine chandle
          cmd <- liftIO $ (S8.pack . trim) `fmap` hGetLine chandle
          envs <- liftIO $ readEnvs chandle
          filesize <- liftIO $ (read . trim) `fmap` hGetLine chandle
          liftIO $ print ("about to get filename")
          filename <- liftIO $ trim `fmap` hGetLine chandle
          liftIO $ print ("about to read file: " ++ filename)
          tarBS <- liftIO $ S.readFile filename -- convert to bytestring
          appId <- liftIO $ modifyMVar jobMutex (\a -> return (a + 1, a))
          let tarwriter dput = dput tarBS
          let job = Job appId appname cmd envs filesize filename tarwriter
          liftIO $ print (appId, appname, cmd, filesize)
          msg <- deployJob job jobMutex nginxMutex
          liftIO $ hPutStrLn chandle msg
      "add" -> do  -- add a new deployer
          -- format:
          -- hostname
          -- deployer's port number
          --liftIO $ do
          hostname <- liftIO $ trim `fmap` hGetLine chandle
          dPort <- liftIO $ trim `fmap` hGetLine chandle
          dhandle <- liftIO $ connectTo hostname $ toPortId dPort -- handle for the app deployer
          deployer <- liftIO $ deployerFromHandle (hostname, read dPort) dhandle
          addDeployer deployer deployerMutex
      "kill" -> do  -- kill an app
          -- format:
          -- app name
          -- appId
          appname <- liftIO $ trim `fmap` hGetLine chandle
          appId <- liftIO $ (read . trim) `fmap` hGetLine chandle
          eresponse <- killJob appId appname jobMutex nginxMutex
          liftIO $ case eresponse of
            Right () -> do  -- success
              hPutStrLn chandle "Done"
            Left error -> hPutStrLn chandle error
      "remove" -> do
          -- remove an app from the ht (deployer will invoke this function when an app dies without the kill command)
          -- format:
          -- app name
          -- appId
          liftIO $ print "appcontroller: remove called"
          appname <- liftIO $ trim `fmap` hGetLine chandle
          appId <- liftIO $ (read . trim) `fmap` hGetLine chandle
          removeJob appId appname jobMutex nginxMutex
      _ -> do
          liftIO $ hPutStrLn chandle $ "INVALID COMMAND: " ++ cmd


-- Utils

foreverOrEOF :: Handle -> Controller () -> Controller ()
foreverOrEOF h act = do
    eof <- liftIO $ hIsEOF h
    if eof then
      return ()
      else do
        act
        foreverOrEOF h act

-- return a hashtable with deployerId's and deployers from the backup file
fillDeployerFromFile :: FilePath -> MVar () -> IO DeployerHt
fillDeployerFromFile filepath mutex = do
  h <- atomic mutex $ openFile filepath ReadWriteMode
  ht <- H.new
  foreverOrEOF2 h $ do
    idStr <- trim `fmap` hGetLine h
    let [hostname, dport] = split "," idStr
        did = (hostname, read dport)
    dhandle <- connectTo hostname $ toPortId dport
    deployer <- deployerFromHandle did dhandle
    mdeployer <- newMVar deployer
    atomic mutex $ H.insert ht did mdeployer  -- hostname = deployer id
  hClose h
  return ht

-- return a hashtable with jobId's and deployers from the backup file
fillJobsFromFile :: FilePath -> MVar Int -> IO JobHt
fillJobsFromFile filepath mutex = do
  h <- atomic mutex $ openFile filepath ReadWriteMode
  ht <- H.new
  foreverOrEOF2 h $ do
    entry <- atomic mutex $ trim `fmap` hGetLine h
    let [jid, appname, cmd, tarsize, tarfile, hostname, deployerPort] = split "," entry
    envs <- readEnvs h
    liftIO $ print ("about to read file: " ++ tarfile)
    tarBS <- S.readFile tarfile  -- convert to bytestring
    let tarwriter dput = dput tarBS
    let job = Job (read jid) (S8.pack appname) (S8.pack cmd) envs (read tarsize) tarfile tarwriter
    dhandle <- connectTo hostname $ toPortId deployerPort
    deployer <- deployerFromHandle (hostname, read deployerPort) dhandle
    mdeployer <- newMVar deployer
    H.insert ht job mdeployer  -- hostname = deployer id
    eof <- hIsEOF h
    if eof then do
      takeMVar mutex
      putMVar mutex (read jid + 1)
      else return ()
  hClose h
  return ht

--type JobHt = H.BasicHashTable JobId (MVar Deployer)  -- job id's and their deployers
--type DeployerHt = H.BasicHashTable DeployerId (MVar Deployer)  -- deployer ids and deployers

    --  send some sort of message (job status?)
    --  if responds successfully, cool
    --  otherwise, it's dead, need to relocate jobs that were assigned to it.
-- Checks to see if a deployer is alive
checkDeployer :: DeployerHt -> JobHt -> MVar Int -> MVar () -> (DeployerId, MVar Deployer) -> IO Bool
checkDeployer deployerht jobht jobMutex nginxMutex (did, mdeployer) = do
  result <- try $ connectTo (fst did) (PortNumber $ toEnum $ snd did)
  case (result  :: Either IOException Handle) of
    Left _ -> do
      print ("Deployer " ++ show did ++ " is down")
      return False
    Right handle -> return True

readEnvs :: Handle -> IO String
readEnvs handle = do
  str <- readEnvHelper handle ""
  trace ("envs to be printed: " ++ str ++ "end of envs") $ return ()
  return str
  where readEnvHelper h envs = do
          line <- trim `fmap` hGetLine h
          case line of
            "" -> return envs
            env -> readEnvHelper h (envs ++ env ++ "\n")

toPortId str = PortNumber $ toEnum $ read str
