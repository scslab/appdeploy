import Control.Exception
import Control.Monad
import System.Process
import Control.Concurrent
import Data.Char
import Data.List.Split
import qualified Data.HashTable.IO as H
import Network
import System.IO
import System.IO.Unsafe
import Debug.Trace

ht :: (H.BasicHashTable Int ProcessHandle)
{-# NOINLINE ht #-}
ht = unsafePerformIO $ H.new

main :: IO ()
main = bracket (listenOn $ PortNumber 9876) sClose $ \s -> do
  htMutex <- newMVar 0
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

handleConnection :: Handle -> MVar Int -> IO ()
handleConnection h htMutex = forever $ do
    hPutStrLn h "Enter Command: "
    cmd <- trim `fmap` hGetLine h
    case cmd of
        "statuses" -> do
            statusList <- atomic htMutex $ H.toList ht 
            hPutStrLn h (show $ map fst statusList)
        "launch" -> do
            shellcmd <- trim `fmap` hGetLine h 
            let env = [] --something from the request
            oldId <- modifyMVar htMutex (\a -> return (a + 1, a))
            startApp htMutex shellcmd oldId
            hPutStrLn h $ show oldId
        "kill" -> atomic htMutex $ do
            key <- (read . trim) `fmap` hGetLine h 
            mPHandle <- H.lookup ht key 
            case mPHandle of
              Nothing -> hPutStrLn h "No process found"
              Just pHandle -> do
                terminateProcess pHandle
                H.delete ht key
        _ -> do
            hPutStrLn h $ "invalid command: (" ++ cmd ++ ")"

startApp :: MVar Int 
            -> String    -- Command
            -> Int       -- Identifier
            -> IO ()
startApp htMutex command identifier = void $ forkIO $ do 
    let createProc = shell command
    pHandle <- atomic htMutex $ do
      (_, _, _, pHandle) <- createProcess createProc
      H.insert ht identifier pHandle
      return pHandle
    waitForProcess pHandle
    mPHandle <- withMVar htMutex $ \_ -> H.lookup ht identifier
    case mPHandle of
      Nothing -> return ()
      Just pHandle -> do
        atomic htMutex $ H.delete ht identifier
        startApp htMutex command identifier

atomic :: MVar b -> IO a -> IO a
atomic mtx act = withMVar mtx $ \_ -> act

--
-- isSpace :: Char -> Bool
-- if the character is one of ' ', '\t', '\n', '\r'...
--

trim :: String -> String
trim str = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse str

