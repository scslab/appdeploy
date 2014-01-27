module Main where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.Char
import qualified Data.HashTable.IO as H
import qualified Data.ByteString.Lazy as L
import Debug.Trace
import Network
import System.IO
import System.IO.Unsafe
import NginxUpdater
import Utils

-- read tar files in & send over network
-- accept commands: "run this app"
-- communicate w/ app deployer about statuses of apps
-- load balancing
-- kill commands
-- have some way of knowing the statuses once this program dies/restarts
-- know the availability of the deployers

appht :: (H.BasicHashTable Int String)  -- app id's and the hostnames of the deployers they run on
{-# NOINLINE appht #-}
appht = unsafePerformIO $ H.new

appFile :: FilePath  -- backup of appht, with key/value pairs separated by commas
appFile = "appinfo.txt"

deployerht :: (H.BasicHashTable String Int)  -- deployer hostnames and their statuses (0 or 1)
{-# NOINLINE deployerht #-}
deployerht = unsafePerformIO $ H.new

deployerFile :: FilePath  -- backup of deployerht, with key/value pairs separated by commas
deployerFile = "deployerinfo.txt"

test = do
  ht <- H.new
  H.insert ht "key" "val"
  fillTable "testht" ht 
  list <- H.toList ht
  trace (show list) $ return ()

main :: IO ()
main = bracket (listenOn $ PortNumber 1234) sClose $ \s -> forever $ do
  appMutex <- newMVar 0  -- for appht
  depMutex <- newMVar ()  -- for deployerht
  (h, _, _) <- accept s
  -- TODO: put info from files into the hashtables
  forkIO $ handleConnection h appMutex depMutex
             `finally` hClose h

handleConnection :: Handle -> MVar Int -> MVar () -> IO ()
handleConnection chandle appMutex depMutex = foreverOrEOF chandle $ do
    let portint = 9876
        portnum = 9876
        port = PortNumber portnum
        nginxfile = "nginx.conf"  -- todo: change this to the proper path
    cmd <- trim `fmap` hGetLine chandle
    case cmd of
        "statuses" -> do  -- show statuses of all app deployers in the hashtable
            deployerList <- atomic depMutex $ H.toList deployerht
            hPutStrLn chandle (show $ map fst deployerList)
            return ()
        "deployer" -> do  -- show statuses of all apps of a deployer
            hostname <- trim `fmap` hGetLine chandle  -- which port the deployer runs on
            dhandle <- connectTo hostname port  -- handle for the app deployer
            hPutStrLn dhandle "statuses"
            statuses <- hGetLine dhandle
            hPutStrLn chandle statuses
        "run" -> do  -- run an app
            -- format:
            -- app name (aka command to be run)
            -- hostname of the deployer it should run on
            -- var1=val1
            -- var2=val2
            -- ...
            --
            -- size of tar file
            -- tar file path
            appname <- trim `fmap` hGetLine chandle
            hostname <- trim `fmap` hGetLine chandle  -- shouldn't be entered by the user
            envs <- readEnvs chandle
            filesize <- trim `fmap` hGetLine chandle  -- todo: get filesize based on tarfile
            filename <- trim `fmap` hGetLine chandle
            tarBS <- L.readFile filename  -- convert to bytestring
            dhandle <- connectTo hostname port  -- handle for the app deployer
            appId <- modifyMVar appMutex (\a -> return (a + 1, a))  -- allows multiple instances of same app to run
            atomic appMutex $ do
              H.insert appht appId hostname
              addToFile appFile (show appId) hostname
            --addEntry nginxfile appname $ DeployInfo appId hostname portint
            addEntry nginxfile "testapp" $ DeployInfo 1 "hi" 9876  -- todo
            hPutStrLn dhandle "launch"
            hPutStrLn dhandle appname
            hPutStrLn dhandle $ show portnum
            hPutStrLn dhandle ("PORT=" ++ show portnum)
            hPutStrLn dhandle envs
            --hPutStrLn dhandle ""
            hPutStrLn dhandle filesize
            L.hPut dhandle tarBS
            trace "finished run command" $ return ()
        "add" -> do  -- add a new deployer
            -- format:
            -- hostname
            hostname <- trim `fmap` hGetLine chandle
            let status = 1
            atomic depMutex $ do
              H.insert deployerht hostname status
              addToFile deployerFile hostname $ show status
        "kill" -> do  -- kill an app
            --removeEntry nginxfile "testapp" $ DeployInfo 1 "hi" 9876
            -- format:
            -- app name
            -- appId
            appname <- trim `fmap` hGetLine chandle
            appId <- (read . trim) `fmap` hGetLine chandle
            mhost <- atomic appMutex $ H.lookup appht appId
            case mhost of
              Nothing -> hPutStrLn chandle "NOT FOUND"
              Just hostname -> do
                dhandle <- connectTo hostname port  -- handle for the app deployer
                hPutStrLn dhandle "kill"
                hPutStrLn dhandle $ show appId
                response <- hGetLine dhandle  -- OK or NOT FOUND
                hPutStrLn chandle response
                removeEntry nginxfile appname $ DeployInfo appId hostname portint
        "remove" -> do
            -- remove an app from the ht (deployer will invoke this function when an app dies without the kill command)
            -- format:
            -- app name
            -- appId
            appname <- trim `fmap` hGetLine chandle
            appId <- (read . trim) `fmap` hGetLine chandle
            mhostname <- atomic appMutex $ H.lookup appht appId
            case mhostname of
              Nothing -> return ()
              Just hostname -> do
                removeEntry nginxfile appname $ DeployInfo appId hostname portint
        _ -> do
            hPutStrLn chandle $ "INVALID COMMAND (" ++ cmd ++ ")"


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

