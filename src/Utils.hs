
module Utils where

import Control.Concurrent
import Data.Char
import Data.String.Utils
import qualified Data.HashTable.IO as H
import Network
import System.IO

-- add one hashtable entry to a file
addToFile :: FilePath -> String -> String -> IO ()
addToFile filepath key value = do  -- NOTE: key must not contain commas
  h <- openFile filepath AppendMode
  hPutStrLn h (key ++ "," ++ value)

-- retrieve all entries from a file and add them to a hashtable
fillTable :: FilePath -> H.BasicHashTable String String -> IO ()
fillTable filepath ht = do
  h <- openFile filepath ReadWriteMode
  foreverOrEOF2 h $ do
    entry <- hGetLine h
    let [key,val] = split "," entry
    H.insert ht key val
    
foreverOrEOF2 :: Handle -> IO () -> IO ()
foreverOrEOF2 h act = do
    eof <- hIsEOF h
    if eof then
      return ()
      else do
        act
        foreverOrEOF2 h act

atomic :: MVar b -> IO a -> IO a
atomic mtx act = withMVar mtx $ \_ -> act

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

