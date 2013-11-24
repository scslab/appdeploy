{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

import Control.Monad.State
import Control.Concurrent.MVar
import qualified Data.HashTable.IO as H
import Data.List
import Data.List.Utils
import DeployInfo
import System.Environment   
import System.Directory  
import System.IO  
import System.IO.Unsafe

instance DeployTable FilePath IO where

{-
  ht :: (H.BasicHashTable AppName DeployInfo)
  {-# NOINLINE ht #-}
  ht = unsafePerformIO $ H.new
-}

  addEntry filepath appname deployinfo = do
    --atomic htMutex $ H.insert ht appname deployinfo
    oldh <- openFile filepath ReadWriteMode
    let tmppath = filepath ++ ".new"
    newh <- openFile tmppath ReadWriteMode
    let starttag = "# START: " ++ appname ++ (show $ identifier deployinfo)
        endtag = "# END: " ++ appname ++ (show $ identifier deployinfo)
    let code = starttag ++
               "    server { \n\
               \        listen 8080; \n\
               \        server_name  task.lvh.me; \n\
               \        location / { \n\
               \           proxy_pass http://localhost:1234; \n\
               \        } \n\
               \    }"
               ++ endtag
    insertAt "http {" code oldh newh
    renameFile filepath (filepath ++ ".backup")
    renameFile tmppath filepath
    where insertAt tag newtext oldh newh = do
            eof <- hIsEOF oldh
            case eof of
              True -> return ()  -- done copying old file into new
              _ -> do
                line <- hGetLine oldh
                hPutStrLn newh line
                if (isInfixOf tag line) then do -- if the http tag is part of the line
                  hPutStrLn newh newtext
                  copy oldh newh  -- copy the rest of the old file into the new file
                  else return ()
                insertAt tag newtext oldh newh

  removeEntry filepath appname deployinfo = do
    --atomic htMutex $ H.delete ht appname
    let starttag = "# START: " ++ appname ++ (show $ identifier deployinfo)
        endtag = "# END: " ++ appname ++ (show $ identifier deployinfo)
    withFile filepath ReadWriteMode $ \handle -> do
      contents <- hGetContents handle
      let allLines = lines contents
          -- everything before start tag
          before = takeWhile (\line -> not $ isInfixOf starttag line) allLines
          -- everything after end tag
          after = drop 1 $ dropWhile (\line -> not $ isInfixOf endtag line) allLines
      let newcontents = before ++ after
      hPutStr handle $ unlines newcontents
      hClose handle
    return ()
            
  lookup filepath appname = do
    return ()

atomic :: MVar b -> IO a -> IO a
atomic mtx act = withMVar mtx $ \_ -> act
