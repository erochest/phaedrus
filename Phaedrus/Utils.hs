

module Phaedrus.Utils
    ( shuffle
    , tshow
    , toText'
    ) where


import           Control.Monad
import           Data.Array.IO
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude                   hiding (FilePath)
import           System.Random.MWC


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

