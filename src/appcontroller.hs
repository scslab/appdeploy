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

deployerht :: (H.BasicHashTable String Int)  -- deployer hostnames and their statuses (0 or 1)
{-# NOINLINE deployerht #-}
deployerht = unsafePerformIO $ H.new

main :: IO ()
main = bracket (listenOn $ PortNumber 1234) sClose $ \s -> forever $ do
  appMutex <- newMVar 0  -- for appht
  depMutex <- newMVar ()  -- for deployerht
  (h, _, _) <- accept s
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
            -- size of tar file
            -- tar file path
            appname <- trim `fmap` hGetLine chandle
            hostname <- trim `fmap` hGetLine chandle  -- shouldn't be entered by the user
            filesize <- trim `fmap` hGetLine chandle  -- todo: get filesize based on tarfile
            filename <- trim `fmap` hGetLine chandle
            tarBS <- L.readFile filename  -- convert to bytestring
            dhandle <- connectTo hostname port  -- handle for the app deployer
            appId <- modifyMVar appMutex (\a -> return (a + 1, a))  -- allows multiple instances of same app to run
            atomic appMutex $ H.insert appht appId hostname
            --addEntry nginxfile appname $ DeployInfo appId hostname portint
            addEntry nginxfile "testapp" $ DeployInfo 1 "hi" 9876
            hPutStrLn dhandle "launch"
            hPutStrLn dhandle appname
            hPutStrLn dhandle $ show portnum
            hPutStrLn dhandle ("PORT=" ++ show portnum)
            hPutStrLn dhandle ""
            hPutStrLn dhandle filesize
            L.hPut dhandle tarBS
            trace "finished run command" $ return ()
        "add" -> do  -- add a new deployer
            -- format:
            -- hostname
            hostname <- trim `fmap` hGetLine chandle
            let status = 1
            atomic depMutex $ H.insert deployerht hostname status
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

foreverOrEOF :: Handle -> IO () -> IO ()
foreverOrEOF h act = do
    eof <- hIsEOF h
    if eof then
      return ()
      else do
        act
        foreverOrEOF h act

atomic :: MVar b -> IO a -> IO a
atomic mtx act = withMVar mtx $ \_ -> act

trim :: [Char] -> [Char]
trim = triml . trimr

triml :: [Char] -> [Char]
triml [] = []
triml arr@(x:xs) =
  if (isSpace x)
    then triml xs
    else arr

trimrhelper :: [Char] -> [Char] -> [Char] -> [Char]
trimrhelper "" accm _ = reverse accm
trimrhelper str accm total =
  let next = ((head str):total)
  in if isSpace $ head str then
       trimrhelper (tail str) accm next
       else trimrhelper (tail str) next next

trimr :: [Char] -> [Char]
trimr []     = []
trimr x = trimrhelper x "" ""
