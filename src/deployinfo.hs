module DeployInfo where

import qualified Data.HashTable.IO as H
import System.IO.Unsafe

type AppName = String

data DeployInfo = DeployInfo {
  hostname :: String,
  portnum :: Int
}

ht :: (H.BasicHashTable AppName DeployInfo)
{-# NOINLINE ht #-}
ht = unsafePerformIO $ H.new

addEntry :: AppName -> DeployInfo -> IO ()
addEntry = H.insert ht

removeEntry :: AppName -> IO ()
removeEntry = H.delete ht

lookup :: AppName -> IO (Maybe DeployInfo)
lookup = H.lookup ht

