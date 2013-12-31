{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module NginxUpdater where

import Data.Maybe
import Control.Monad.State
import Control.Concurrent.MVar
import Debug.Trace
import qualified Data.HashTable.IO as H
import Data.List
import Data.List.Utils
import System.Environment   
import System.Directory  
import System.IO  
import System.IO.Unsafe
import Text.Regex

type AppName = String

data DeployInfo = DeployInfo {
  identifier :: Int,
  hostname :: String,
  portnum :: Int
} deriving Show

class Monad m => Table d m where
  addEntry :: d -> AppName -> DeployInfo -> m ()
  removeEntry :: d -> AppName -> DeployInfo -> m ()
  lookup :: d -> AppName -> m (Maybe DeployInfo)


instance Table FilePath IO where

  addEntry filepath appname deployinfo = do
    oldh <- openFile filepath ReadWriteMode  -- handle to current config file
    let tmppath = filepath ++ ".new"  -- temporary; will be copied back to filepath at the end
    newh <- openFile tmppath ReadWriteMode  -- handle to modified config file
    let starttag = "# START: " ++ appname ++ (show $ identifier deployinfo)
        endtag = "# END: " ++ appname ++ (show $ identifier deployinfo)
    let code = starttag ++ "\n" ++  -- new code to add to the config file
               "    server { \n\
               \        listen 8080; \n\
               \        server_name  task.lvh.me; \n " ++  -- todo: change this to the actual url
               "        location / { \n\
               \           proxy_pass http://localhost:1234; \n\
               \        } \n\
               \    } \n"
               ++ endtag
    insertAt "http {" code oldh newh
    renameFile filepath (filepath ++ ".backup")
    renameFile tmppath filepath
    where insertAt tag newtext oldh newh = do
            eof <- hIsEOF oldh
            if eof then do
              hPutStrLn newh "http {"
              hPutStrLn newh newtext
              hPutStrLn newh "}"
              hClose newh
              return ()
              else do
                line <- hGetLine oldh
                hPutStrLn newh line
                if (isInfixOf tag line) then do -- if the http tag is part of the line
                  hPutStrLn newh newtext
                  copyRemainder oldh newh  -- copy the rest of the old file into the new file
                  else insertAt tag newtext oldh newh
          copyRemainder oldh newh = do  -- copy the remainder of oldh's file into newh's file
            eof <- hIsEOF oldh
            if eof then return () else do
              line <- hGetLine oldh
              hPutStrLn newh line
              copyRemainder oldh newh

  removeEntry filepath appname deployinfo = do
    let starttag = "# START: " ++ appname ++ (show $ identifier deployinfo)
        endtag = "# END: " ++ appname ++ (show $ identifier deployinfo)
    let backuppath = filepath ++ ".backup"
    renameFile filepath backuppath
    oldh <- openFile backuppath ReadWriteMode
    newh <- openFile filepath ReadWriteMode
    processFile oldh newh starttag endtag True
    hClose oldh
    hClose newh
    return ()
    where processFile oldh newh starttag endtag copymode = do
            eof <- hIsEOF oldh
            if eof then return () else do
              line <- hGetLine oldh
              if (isInfixOf starttag line || isInfixOf endtag line) then
                processFile oldh newh starttag endtag $ not copymode
                else do
                  if copymode then hPutStrLn newh line
                    else return ()
                  processFile oldh newh starttag endtag copymode

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
                  else getDeployInfo h identifier mhostname
              Nothing ->  -- hostname is still unknown; find the hostname
                if (isInfixOf "server_name" line) then
                  let pattern = mkRegex "server_name[[:space:]]+([a-z[:punct:]]+);"
                      hostname = head $ fromJust $ matchRegex pattern line
                  in getDeployInfo h identifier $ Just hostname
                  else getDeployInfo h identifier Nothing

