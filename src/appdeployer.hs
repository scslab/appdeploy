module Main where

import Control.Exception
import Control.Monad
import System.FilePath
import System.Directory
import System.Process
import Control.Concurrent
import Data.Char
import Data.List.Split
import Debug.Trace
import qualified Data.HashTable.IO as H
import qualified Data.ByteString.Lazy as L
import Data.Time.Clock
import qualified Codec.Archive.Tar as Tar
import Network
import System.IO.Temp
import System.IO
import System.IO.Unsafe

ht :: (H.BasicHashTable Int ProcessHandle)
{-# NOINLINE ht #-}
ht = unsafePerformIO $ H.new

tmpDir :: FilePath
tmpDir = "tmp"

main :: IO ()
main = bracket (listenOn $ PortNumber 9876) sClose $ \s -> do
  exists <- doesDirectoryExist tmpDir
  when exists $ removeDirectoryRecursive tmpDir
  createDirectory tmpDir

  htMutex <- newMVar 0
  forever $ do
    (handle, hostname, portnum) <- accept s
    forkIO $ handleConnection handle htMutex
               `finally` hClose handle

foreverOrEOF :: Handle -> IO () -> IO ()
foreverOrEOF h act = do
    isEOF <- hIsEOF h
    if isEOF then
      return ()
      else do
        act
        foreverOrEOF h act

handleConnection :: Handle -> MVar Int -> IO ()
handleConnection h htMutex = foreverOrEOF h $ do
    cmd <- trim `fmap` hGetLine h
    case cmd of
        "statuses" -> do  -- prints list of processes
            statusList <- atomic htMutex $ H.toList ht 
            hPutStrLn h (show $ map fst statusList)
        "launch" -> do  -- prints pid
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
            id <- (read . trim) `fmap` hGetLine h 
            env <- readenvs h
            nbytes <- read `fmap` hGetLine h
            tarfile <- L.hGet h nbytes
            let entries = Tar.read tarfile
            tmppath <- createTempDirectory tmpDir "appdeploy"
            Tar.unpack tmppath entries
            --oldId <- modifyMVar htMutex (\a -> return (a + 1, a))
            --void $ forkIO $ startApp htMutex shellcmd env tmppath oldId 0
            void $ forkIO $ startApp htMutex shellcmd env tmppath id 0
        "kill" -> atomic htMutex $ do  -- OK or NOT FOUND
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
startApp htMutex command env cwd identifier retries = when (retries < 5) $ do 
    stdout <- openFile (cwd </> "log.out") AppendMode
    stderr <- openFile (cwd </> "log.err") AppendMode
    stdin <- openFile "/dev/null" ReadMode
    let createProc = (shell command) { env = Just env
                                     , cwd = Just cwd
                                     , std_in = UseHandle stdin
                                     , std_out = UseHandle stdout
                                     , std_err = UseHandle stderr }
    pHandle <- atomic htMutex $ do
      (_, _, _, pHandle) <- createProcess createProc
      hClose stdout
      hClose stderr
      hClose stdin
      H.insert ht identifier pHandle
      return pHandle
    startTime <- getCurrentTime
    waitForProcess pHandle
    endTime <- getCurrentTime
    mPHandle <- withMVar htMutex $ \_ -> H.lookup ht identifier
    case mPHandle of
      Nothing -> removeDirectoryRecursive cwd
      Just pHandle -> do
        atomic htMutex $ H.delete ht identifier
        let nextRetries = if (diffUTCTime endTime startTime < 30) then
                            retries + 1
                            else 0
        startApp htMutex command env cwd identifier nextRetries

readenvs :: Handle -> IO [(String,String)]
readenvs h = go h []
  where go h list = do
          line <- trim `fmap` hGetLine h
          putStrLn line
          if line == "" then
            return $ reverse list
            else go h $ (parseEnv line):list

parseEnv :: String -> (String, String)
parseEnv envString =
  let (key:value:[]) = splitOn "=" envString
  in (key, value)


atomic :: MVar b -> IO a -> IO a
atomic mtx act = withMVar mtx $ \_ -> act

--
-- isSpace :: Char -> Bool
-- if the character is one of ' ', '\t', '\n', '\r'...
--

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

