module Main where

import Control.Exception
import Control.Monad
import System.FilePath
import System.Directory
import System.Process
import Control.Concurrent
import Data.List.Split
import qualified Data.HashTable.IO as H
import qualified Data.ByteString.Lazy as L
import Data.Time.Clock
import qualified Codec.Archive.Tar as Tar
import Network
import System.Environment
import System.IO
import System.IO.Unsafe
import Utils

-- hashtable of app identifiers and process handles
ht :: (H.BasicHashTable Int ProcessHandle)
{-# NOINLINE ht #-}
ht = unsafePerformIO $ H.new

tmpDir :: FilePath
tmpDir = "tmp"

main :: IO ()
main = do
  portstr <- head `fmap` getArgs
  let port = PortNumber $ toEnum $ read portstr
  bracket (listenOn port) sClose $ \s -> do
    exists <- doesDirectoryExist tmpDir
    when exists $ removeDirectoryRecursive tmpDir
    createDirectory tmpDir

    htMutex <- newMVar 0
    forever $ do
      (h, _, _) <- accept s
      forkIO $ handleConnection h htMutex
                 `finally` hClose h

handleConnection :: Handle -> MVar Int -> IO ()
handleConnection h htMutex = foreverOrEOF2 h $ do
    cmd <- trim `fmap` hGetLine h
    case cmd of
        "statuses" -> do  -- prints list of processes
            statusList <- atomic htMutex $ H.toList ht 
            hPutStrLn h (show $ map fst statusList)
        "launch" -> do  -- prints pid
            putStrLn "launch called!"
            -- format:
            -- shell cmd
            -- identifier (as an int)
            -- var1=val1
            -- var2=val2
            -- ...
            --
            -- num bytes
            -- tar data
            shellcmd <- trim `fmap` hGetLine h 
            putStrLn $ "SHELL: " ++ (show shellcmd)
            identifier <- (read . trim) `fmap` hGetLine h 
            putStrLn $ "ID: " ++ (show (identifier :: Int))
            envs <- readenvs h
            nbytes <- read `fmap` hGetLine h
            tarfile <- L.hGet h nbytes
            let entries = Tar.read tarfile
            Tar.unpack tmpDir entries
            void $ forkIO $ startApp htMutex shellcmd envs tmpDir identifier 0
        "kill" -> atomic htMutex $ do  -- OK or NOT FOUND
            -- format:
            -- app identifier
            key <- (read . trim) `fmap` hGetLine h 
            mPHandle <- H.lookup ht key 
            case mPHandle of
              Nothing -> hPutStrLn h "NOT FOUND"
              Just pHandle -> do
                terminateProcess pHandle
                H.delete ht key
                hPutStrLn h "OK"
        _ -> do
            hPutStrLn h $ "INVALID COMMAND (" ++ cmd ++ ")"

startApp :: MVar Int 
            -> String             -- Command
            -> [(String, String)] -- Environment
            -> FilePath           -- cwd
            -> Int                -- Identifier
            -> Int                -- Retries
            -> IO ()
startApp htMutex command envs cwdpath identifier retries = when (retries < 5) $ do 
    output <- openFile (cwdpath </> "log.out") AppendMode
    err <- openFile (cwdpath </> "log.err") AppendMode
    input <- openFile "/dev/null" ReadMode
    let createProc = (shell command) { env = Just envs
                                     , cwd = Just cwdpath
                                     , std_in = UseHandle input
                                     , std_out = UseHandle output
                                     , std_err = UseHandle err }
    pHandle <- atomic htMutex $ do
      (_, _, _, pHandle) <- createProcess createProc
      hClose output
      hClose err
      hClose input
      H.insert ht identifier pHandle
      return pHandle
    startTime <- getCurrentTime
    _ <- waitForProcess pHandle
    endTime <- getCurrentTime
    mPHandle <- withMVar htMutex $ \_ -> H.lookup ht identifier
    case mPHandle of
      Nothing -> removeDirectoryRecursive cwdpath
      Just _ -> do
        atomic htMutex $ H.delete ht identifier
        removeFromController command identifier
        let nextRetries = if (diffUTCTime endTime startTime < 30) then
                            retries + 1
                            else 0
        startApp htMutex command envs cwdpath identifier nextRetries


-- Utils

removeFromController :: Show a => String -> a -> IO ()
removeFromController appname identifier = do
  let hostname = "localhost"  -- hostname of the app controller
      port = PortNumber 1234  -- port of the app controller
  h <- connectTo hostname port  -- handle for the app controller
  hPutStrLn h "remove"
  hPutStrLn h appname
  hPutStrLn h $ show identifier

readenvs :: Handle -> IO [(String,String)]
readenvs h = go h []
  where go hand list = do
          line <- trim `fmap` hGetLine hand
          putStrLn line
          if line == "" then
            return $ reverse list
            else go hand $ (parseEnv line):list

parseEnv :: String -> (String, String)
parseEnv envString =
  let (key:value:[]) = splitOn "=" envString
  in (key, value)

