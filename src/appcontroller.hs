module Main where

import Control.Exception
import Control.Monad
import System.FilePath
import System.Directory
import System.Process
import Control.Concurrent
import Data.Char
import Data.List.Split
import qualified Data.HashTable.IO as H
import qualified Data.ByteString.Lazy as L
import Data.Time.Clock
import qualified Codec.Archive.Tar as Tar
import Network
import System.IO.Temp
import System.IO
import System.IO.Unsafe

-- read tar files in & send over network
-- accept commands: "run this app"
-- communicate w/ app deployer about statuses of apps
-- load balancing
-- kill commands
-- have some way of knowing the statuses once this program dies/restarts
-- know the availability of the deployers

appht :: (H.BasicHashTable Int String)  -- hashtable of process id's and the hostnames of the deployers they run on
{-# NOINLINE appht #-}
appht = unsafePerformIO $ H.new

deployerht :: (H.BasicHashTable String Int)  -- hashtable of deployer hostnames and their statuses (as ints for now)
{-# NOINLINE deployerht #-}
deployerht = unsafePerformIO $ H.new

main :: IO ()
main = bracket (listenOn $ PortNumber 1234) sClose $ \s -> forever $ do
  appMutex <- newMVar 0  -- for appht
  depMutex <- newMVar ()  -- for deployerht
  (handle, hostname, portnum) <- accept s
  forkIO $ handleConnection handle appMutex depMutex
             `finally` hClose handle

handleConnection :: Handle -> MVar Int -> MVar () -> IO ()
handleConnection chandle appMutex depMutex = foreverOrEOF chandle $ do
    let port = PortNumber 9876
        portnum = 9876
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
            -- app name
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
            hPutStrLn dhandle "launch"
            hPutStrLn dhandle appname
            hPutStrLn dhandle $ show portnum
            hPutStrLn dhandle ("PORT=" ++ show portnum)
            hPutStrLn dhandle ""
            hPutStrLn dhandle filesize
            L.hPut dhandle tarBS
        "add" -> do  -- add a new deployer
            hostname <- trim `fmap` hGetLine chandle
            let status = 1
            atomic depMutex $ H.insert deployerht hostname status
        "kill" -> do  -- kill an app
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
        _ -> do
            hPutStrLn chandle $ "INVALID COMMAND (" ++ cmd ++ ")"

-- Utils

foreverOrEOF :: Handle -> IO () -> IO ()
foreverOrEOF h act = do
    isEOF <- hIsEOF h
    if isEOF then
      return ()
      else do
        act
        foreverOrEOF h act

atomic :: MVar b -> IO a -> IO a
atomic mtx act = withMVar mtx $ \_ -> act

trim = triml . trimr

triml [] = []
triml arr@(x:xs) =
  if (isSpace x)
    then triml xs
    else arr

trimrhelper "" accm _ = reverse accm
trimrhelper str accm total =
  let next = ((head str):total)
  in if isSpace $ head str then
       trimrhelper (tail str) accm next
       else trimrhelper (tail str) next next


trimr []     = []
trimr x = trimrhelper x "" ""
