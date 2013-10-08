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

ht :: (H.BasicHashTable Int Int)  -- hashtable of app deployers and their statuses (as ints for now)
{-# NOINLINE ht #-}
ht = unsafePerformIO $ H.new

main :: IO ()
main = bracket (listenOn $ PortNumber 1234) sClose $ \s -> forever $ do
  (chandle, hostname, portnum) <- accept s  -- handle for communicating with the client
  appname <- trim `fmap` hGetLine chandle
  port <- trim `fmap` hGetLine chandle
  tarfile <- trim `fmap` hGetLine chandle
  filesize <- trim `fmap` hGetLine chandle
  dhandle <- connectTo hostname $ PortNumber 9876  -- handle for the app deployer
  hPutStrLn dhandle "launch"
  hPutStrLn dhandle appname
  hPutStrLn dhandle ("PORT=" ++ port)
  hPutStrLn dhandle ""
  hPutStrLn dhandle filesize  -- todo: get file size from the tar file
  hPutStrLn dhandle tarfile

handleConnection :: Handle -> MVar Int -> IO ()
handleConnection h htMutex = foreverOrEOF h $ do
    cmd <- trim `fmap` hGetLine h
    case cmd of
        "deployers" -> do
            -- show statuses of all app deployers in the hashtable
            deployerList <- atomic htMutex $ H.toList ht 
            hPutStrLn h (show $ map fst deployerList)
        "run" -> do
            appname <- trim `fmap` hGetLine h 
            -- assign the app to an app deployer
            return ()
        "kill" -> do
            id <- trim `fmap` hGetLine h 
            -- find the app deployer running the process and give it the id
            return ()
        _ -> do
            hPutStrLn h $ "INVALID COMMAND (" ++ cmd ++ ")"

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

