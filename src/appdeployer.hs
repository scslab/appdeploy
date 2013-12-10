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

-- hashtable of app identifiers and process handles
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
    (h, _, _) <- accept s
    forkIO $ handleConnection h htMutex
               `finally` hClose h

foreverOrEOF :: Handle -> IO () -> IO ()
foreverOrEOF h act = do
    eof <- hIsEOF h
    if eof then
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
            trace "launch called" $ return ()
            shellcmd <- trim `fmap` hGetLine h 
            identifier <- (read . trim) `fmap` hGetLine h 
            envs <- readenvs h
            nbytes <- read `fmap` hGetLine h
            trace ("nbytes: " ++ show nbytes) $ return ()
            tarfile <- L.hGet h nbytes
            let entries = Tar.read tarfile
            --createDirectory tmpDir
            Tar.unpack tmpDir entries
            --tmppath <- createTempDirectory tmpDir "appdeploy"
            --Tar.unpack tmppath entries
            --void $ forkIO $ startApp htMutex shellcmd envs tmppath identifier 0
            void $ forkIO $ startApp htMutex shellcmd envs tmpDir identifier 0
            --oldId <- modifyMVar htMutex (\a -> return (a + 1, a))
            --void $ forkIO $ startApp htMutex shellcmd env tmppath oldId 0
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
startApp htMutex command envs cwdpath identifier retries = when (retries < 5) $ do 
    trace "startApp called" $ return ()
    output <- openFile (cwdpath </> "log.out") AppendMode
    err <- openFile (cwdpath </> "log.err") AppendMode
    input <- openFile "/dev/null" ReadMode
    let createProc = (shell command) { env = Just envs
                                     , cwd = Just cwdpath
                                     , std_in = UseHandle input
                                     , std_out = UseHandle output
                                     , std_err = UseHandle err }
    h <- openFile "log" ReadWriteMode
    pHandle <- atomic htMutex $ do
      (_, _, _, pHandle) <- createProcess createProc
      hClose stdout
      hClose stderr
      hClose stdin
      H.insert ht identifier pHandle
      hPutStrLn h "hi"
      hPutStrLn h "inserted id into hashtable"
      return pHandle
    statusList <- atomic htMutex $ H.toList ht 
    hPutStrLn h ("hashtable: " ++ (show $ map fst statusList))
    hClose h
    startTime <- getCurrentTime
    _ <- waitForProcess pHandle
    endTime <- getCurrentTime
    mPHandle <- withMVar htMutex $ \_ -> H.lookup ht identifier
    case mPHandle of
      Nothing -> removeDirectoryRecursive cwdpath
      Just _ -> do
        atomic htMutex $ H.delete ht identifier
        let nextRetries = if (diffUTCTime endTime startTime < 30) then
                            retries + 1
                            else 0
        trace "end of startApp" $ startApp htMutex command envs cwdpath identifier nextRetries

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


atomic :: MVar b -> IO a -> IO a
atomic mtx act = withMVar mtx $ \_ -> act

--
-- isSpace :: Char -> Bool
-- if the character is one of ' ', '\t', '\n', '\r'...
--

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

