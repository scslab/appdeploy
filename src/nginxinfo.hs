{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

import Data.Maybe
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
import Text.Regex

instance Table FilePath IO where

{-
  ht :: (H.BasicHashTable AppName DeployInfo)
  {-# NOINLINE ht #-}
  ht = unsafePerformIO $ H.new
-}

  addEntry filepath appname deployinfo = do
    --atomic htMutex $ H.insert ht appname deployinfo
    oldh <- openFile filepath ReadWriteMode  -- handle to current config file
    let tmppath = filepath ++ ".new"  -- temporary; will be copied back to filepath at the end
    newh <- openFile tmppath ReadWriteMode  -- handle to modified config file
    let starttag = "# START: " ++ appname ++ (show $ identifier deployinfo)
        endtag = "# END: " ++ appname ++ (show $ identifier deployinfo)
    let code = starttag ++  -- new code to add to the config file
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
            if eof then return () else do
              line <- hGetLine oldh
              hPutStrLn newh line
              if (isInfixOf tag line) then do -- if the http tag is part of the line
                hPutStrLn newh newtext
                copyRemainder oldh newh  -- copy the rest of the old file into the new file
                else return ()
              insertAt tag newtext oldh newh
          copyRemainder oldh newh = do  -- copy the remainder of oldh's file into newh's file
            eof <- hIsEOF oldh
            if eof then return () else do
              line <- hGetLine oldh
              hPutStrLn newh line
              copyRemainder oldh newh

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
    -- todo: decide whether to add the app identifier as a parameter (there could be multiple instances of an app running)
    handle <- openFile filepath ReadWriteMode
    lookuphelper appname handle
    where lookuphelper app h = do
            -- this looks for the start tag. if found, it calls getDeployInfo to get the deploy info; if not, return Nothing because the app is not running.
            eof <- hIsEOF h
            if eof then return Nothing
              else do
                line <- hGetLine h
                let starttag = "# START: " ++ appname
                if (isInfixOf starttag line) then do
                  let matches = matchRegex (mkRegex (appname ++ "([0-9]+)")) line
                      identifier = read $ head $ fromJust matches
                  getDeployInfo h identifier Nothing -- found the start tag; now get the deploy info
                  else lookuphelper app h
          getDeployInfo h identifier mhostname = do
            -- parse an app's config info to get its deploy info
            line <- hGetLine h
            case mhostname of
              Just hostname ->  -- found the hostname; now find the port number
                if (isInfixOf "proxy_pass" line) then
                  let matches = fromJust $ matchRegex (mkRegex "http://[a-zA-Z0-9]+:([0-9]+);") line
                  -- matches = ["1234"] or whatever the portnum is
                  in return $ Just $ DeployInfo identifier hostname $ read $ head matches
                  else getDeployInfo h identifier Nothing
              Nothing ->  -- hostname is still unknown; find the hostname
                if (isInfixOf "server_name" line) then
                  let pattern = mkRegex "server_name[[:space:]]+([a-z[:punct:]]+);"
                      hostname = head $ fromJust $ matchRegex pattern line
                  in getDeployInfo h identifier $ Just hostname
                  else getDeployInfo h identifier Nothing

atomic :: MVar b -> IO a -> IO a
atomic mtx act = withMVar mtx $ \_ -> act
