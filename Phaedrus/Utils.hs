

module Phaedrus.Utils
    ( shuffle
    , tshow
    , toText'
    , phaedrusIO
    , putStrLn'
    , createTree'
    , ls
    ) where


import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Array.IO
import qualified Data.Text                 as T
import           Filesystem
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude                   hiding (FilePath)
import           System.Random.MWC

import           Phaedrus.Types


-- | This shuffles a list. This implementation is adapted from
-- http://www.haskell.org/haskellwiki/Random_shuffle.
shuffle :: [a] -> IO [a]
shuffle xs = withSystemRandom . asGenIO $ \gen -> do
    ar <- newArray' n xs
    forM [1..n] $ \i -> do
        j  <- uniformR (i, n) gen
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where n = length xs

          newArray' :: Int -> [a] -> IO (IOArray Int a)
          newArray' n xs = newListArray (1, n) xs


tshow :: Show a => a -> T.Text
tshow = T.pack . show

toText' :: FilePath -> T.Text
toText' = either id id . FS.toText

data Hole = Hole

phaedrusIO :: IO a -> Phaedrus a
phaedrusIO = Phaedrus . scriptIO

putStrLn' :: String -> Phaedrus ()
putStrLn' = phaedrusIO . putStrLn

createTree' :: FilePath -> Phaedrus ()
createTree' = liftIO . createTree

ls :: FilePath -> Phaedrus [FilePath]
ls = liftIO . listDirectory

