import Control.Exception
import Control.Monad
import System.Process
import Control.Concurrent
import Data.List.Split
import qualified Data.HashTable.IO as H
import Network
import System.IO
import System.IO.Unsafe
import Debug.Trace

ht :: (H.BasicHashTable String ProcessHandle)
{-# NOINLINE ht #-}
ht = unsafePerformIO $ H.new

main :: IO ()
main = bracket (listenOn $ PortNumber 1234) sClose $ \s -> do
  htMutex <- newMVar ()
  forever $ do
    (handle, hostname, portnum) <- accept s
    forkIO $ handleConnection handle htMutex


parseEnv :: String -> [(String, String)]
parseEnv envString =
    let varList = lines envString
    in map parseLine varList  
    where parseLine line =
            let (key:value:[]) = splitOn "=" line
            in (key, value)

handleConnection :: Handle -> MVar () -> IO ()
handleConnection h htMutex = forever $ do
    hPutStrLn h "Enter Command: "
    input <- hGetLine h 
    let (cmd:args) = words input
    case cmd of
        "statuses" -> do
            statusList <- withMVar htMutex $ \_ -> H.toList ht 
            hPutStrLn h (show $ map fst statusList)
        "launch" -> do
            let env = [] --something from the request
            startApp htMutex (head args) False (tail args) env
            return ()
        "kill" -> do
            let key = head args
            mPHandle <- withMVar htMutex $ \_ -> H.lookup ht key 
            case mPHandle of
              Nothing -> hPutStrLn h "No process found"
              Just pHandle -> terminateProcess pHandle
        _ -> do
            hPutStrLn h "invalid command"

startApp :: MVar () -> FilePath             -- ^ Command
            -> Bool             -- ^ Search PATH?
            -> [String]             -- ^ Arguments
            -> [(String, String)]     -- ^ Environment
            -> IO ThreadId
startApp htMutex command spath args env  = forkIO $ do 
    let createProc = proc command args
    (_, _, _, pHandle) <- createProcess createProc
    withMVar htMutex $ \_ -> H.insert ht command pHandle
    waitForProcess pHandle
    withMVar htMutex $ \_ -> H.delete ht command

