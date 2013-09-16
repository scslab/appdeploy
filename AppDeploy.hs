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
            env <- readenvs h
            oldId <- modifyMVar htMutex (\a -> return (a + 1, a))
            startApp htMutex shellcmd env oldId
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
            -> String             -- Command
            -> [(String, String)] -- Environment
            -> Int                -- Identifier
            -> IO ()
startApp htMutex command env identifier = void $ forkIO $ do 
    let createProc = (shell command) { env = Just env }
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
        startApp htMutex command env identifier

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

