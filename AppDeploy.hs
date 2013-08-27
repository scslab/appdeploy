{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}

import System.Posix.Process
import System.Posix.Unistd
import System.Posix.Types
import Control.Concurrent
import Data.List.Split
import Data.IORef
import Data.Global
import Control.Concurrent.Spawn
import System.IO.Unsafe
import qualified Data.HashTable as H
import Network
import System.IO
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Exception.Base

import Debug.Trace

-- Step 1
-- taskctl /path/to/app [arg1 [arg2 [arg3]]]
-- look at .env file to get environment variables (?)

-- Step 2
-- taskctl description_file
-- File looks like:
-- /path/to/app1 [arg1 [arg2 [...]]]
-- /path/to/app2 [arg1 [...]]
-- ...

-- Step 2
-- Turn into TCP or HTTP server that can monitor multiple apps

-- Step 3
-- List status of running processes


--type HashTable k v = H.BasicHashTable k v

ht :: (H.HashTable String String)
{-# NOINLINE ht #-}
ht = unsafePerformIO $ H.new (==) H.hashString

--main :: FilePath -> IO ()
main = bracket (listenOn $ PortNumber 1234) sClose $ \s -> do
  htMutex <- newMutex
  --trace "releaseMutex" $ releaseMutex htMutex
  processesString <- trace "processing file" $ readFile "./processes" 
  let processList = lines processesString
  mapM (taskctl htMutex) processList
  forever $ do
    (handle, hostname, portnum) <- accept s
    forkIO $ respondStatuses handle htMutex


--main = do 
--    putStrLn "hello"

    --startApp "/aadfasdf" False ["a"] (Just [("s", "c")])

parseEnv :: FilePath -> IO (Maybe [(String, String)])
parseEnv envFile = do
    envString <- readFile envFile
    let varList = lines envString
    return $ Just $ map (\line -> ((head $ splitOn "=" line) ,(last $ splitOn "=" line))) varList  

--readProcesses :: FilePath -> IO a 
--readProcesses descriptionFile = do
    
--    processesString <- readFile descriptionFile 
--    let processList = lines processesString
--    mapM (taskctl htMutex) processList

taskctl :: Mutex -> String -> IO ThreadId
taskctl htMutex cline = do
    let entries = splitOn " " cline
    let command = head entries
    let args = tail entries
    env <- parseEnv "./.env"
    startApp htMutex command False args env




startApp :: Mutex -> FilePath             -- ^ Command
            -> Bool             -- ^ Search PATH?
            -> [String]             -- ^ Arguments
            -> Maybe [(String, String)]     -- ^ Environment
            -> IO ThreadId
startApp htMutex command spath args env  = forkIO $ go 
        where go = do
                --ht1 <- ht
                --ht2 <- readIORef ht1

                claimMutex htMutex
                success <- H.update ht (show command) "running"
                if not success
                then  H.insert ht (show command) "running"
                else return ()
                releaseMutex htMutex
                err1 <- spawn (executeFile command spath args env)
                claimMutex htMutex
                H.update ht (show command) "down"
                releaseMutex htMutex
                err <- err1
                case err of
                    Left (SomeException e) -> putStrLn (show e)
                    _ -> return ()
                go
        --pid <- forkProcess (executeFile command spath args env) 
        --checkStatus pid command spath args env



respondStatuses :: Handle -> Mutex -> IO ()
respondStatuses h htMutex= do
    --ht1 <- ht
    --ht2 <- readIORef ht1
    hPutStrLn h "Enter Command: "
    input <- hGetLine h 
    --determine command and respond appropriately
    let cmd = head $ words input
    let args = tail $ words input
    env <- parseEnv "./.env"
    case cmd of
        "statuses" -> do
            claimMutex htMutex
            statusList <- H.toList ht 
            releaseMutex htMutex
            hPutStrLn h (show (statusList :: [(String, String)]))
        "launch" -> do
            startApp htMutex (head args) False (tail args) env
            return ()
        "setEnvFile" -> do
            newEnv <- parseEnv (head args)
            startApp htMutex (head args) False (tail args) newEnv
            return ()
        _ -> do
            hPutStrLn h "invalid command"
    respondStatuses h htMutex                



type Mutex = MVar ()

claimMutex :: MonadIO m => Mutex -> m ()
claimMutex = liftIO . takeMVar

newMutex :: MonadIO m => m Mutex
newMutex = liftIO $ newMVar ()

releaseMutex :: MonadIO m => Mutex -> m ()
releaseMutex m = liftIO $ putMVar m ()