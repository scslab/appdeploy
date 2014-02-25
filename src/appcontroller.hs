module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Char
import qualified Data.HashTable.IO as H
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Debug.Trace
import Network
import System.IO
import System.IO.Unsafe
import Deploy.Controller
import NginxUpdater
import Utils

-- TODO: figure out what to do with these files now that the hashtables are gone
appFile :: FilePath  -- backup of appht, with key/value pairs separated by commas
appFile = "appinfo.txt"

deployerFile :: FilePath  -- backup of deployerht, with key/value pairs separated by commas
deployerFile = "deployerinfo.txt"

main :: IO ()
main = bracket (listenOn $ PortNumber 1234) sClose $ \s -> forever $ do
  appMutex <- newMVar 0  -- for appht
  depMutex <- newMVar ()  -- for deployerht
  (h, _, _) <- accept s
  -- TODO: put info from files into the hashtables
  jobs <- H.new
  deployers <- H.new
  let state = ControllerState jobs deployers
  forkIO $ do
    x <- execStateT (handleConnection h appMutex depMutex) state
    hClose h
  {-
  (forkIO $ do
     x <- execStateT (handleConnection h appMutex depMutex) state
     return ())
     `finally` hClose h
  -}
  return ()

handleConnection :: Handle -> MVar Int -> MVar () -> Controller ()
handleConnection chandle appMutex depMutex = foreverOrEOF chandle $ do
    let portint = 9876
        portnum = 9876
        port = PortNumber portnum
    cmd <- liftIO $ trim `fmap` hGetLine chandle
    case cmd of
      "statuses" -> do  -- show statuses of all app deployers in the hashtable
          deployerht <- gets ctrlDeployers  -- ht of dId's and deployers
          liftIO $ do
            deployerList <- H.toList deployerht
            mdeployers <- mapM (takeMVar . snd) deployerList  -- list of mvar deployers
            hPutStrLn chandle $ show $ map deployerId mdeployers  -- TODO
      "deployer" -> do  -- show statuses of all apps of a deployer
          -- format:
          -- hostname
          hostname <- liftIO $ trim `fmap` hGetLine chandle 
          liftIO $ trace (show hostname) $ return ()
          estats <- deployerStats hostname
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
          envs <- liftIO $ readEnvs chandle  -- not supported by controller at the moment
          filesize <- liftIO $ (read . trim) `fmap` hGetLine chandle
          filename <- liftIO $ trim `fmap` hGetLine chandle
          tarBS <- liftIO $ S.readFile filename  -- convert to bytestring
          appId <- liftIO $ modifyMVar appMutex (\a -> return (a + 1, a))
          let tarwriter dput = dput tarBS
          let job = Job appId appname cmd filesize tarwriter
          trace "deploying job" $ deployJob job
      "add" -> do  -- add a new deployer
          -- format:
          -- hostname
          --liftIO $ do
          hostname <- liftIO $ trim `fmap` hGetLine chandle
          dhandle <- liftIO $ connectTo hostname port  -- handle for the app deployer
          deployer <- liftIO $ deployerFromHandle hostname dhandle
          addDeployer deployer
      "kill" -> do  -- kill an app
          -- format:
          -- app name
          -- appId
          appname <- liftIO $ trim `fmap` hGetLine chandle
          appId <- liftIO $ (read . trim) `fmap` hGetLine chandle
          eresponse <- killJob appId
          liftIO $ case eresponse of
            Right () -> do  -- success
              hPutStrLn chandle "Done"
            Left error -> hPutStrLn chandle error
      "remove" -> do
          -- remove an app from the ht (deployer will invoke this function when an app dies without the kill command)
          -- format:
          -- app name
          -- appId
          appname <- liftIO $ trim `fmap` hGetLine chandle
          appId <- liftIO $ (read . trim) `fmap` hGetLine chandle
          removeJob appId
      _ -> do
          liftIO $ hPutStrLn chandle $ "INVALID COMMAND: " ++ cmd


-- Utils

readEnvs :: Handle -> IO String
readEnvs h = do
  str <- readEnvHelper h ""
  trace ("envs to be printed: " ++ str ++ "end of envs") $ return ()
  return str

readEnvHelper h envs = do
  line <- trim `fmap` hGetLine h
  case line of
    "" -> return envs
    env -> readEnvHelper h (envs ++ env)

test = do
  ht <- H.new
  H.insert ht "key" "val"
  fillTable "testht" ht 
  list <- H.toList ht
  trace (show list) $ return ()

foreverOrEOF :: Handle -> Controller () -> Controller ()
foreverOrEOF h act = do
    eof <- liftIO $ hIsEOF h
    if eof then
      return ()
      else do
        act
        foreverOrEOF h act

