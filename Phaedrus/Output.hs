{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Phaedrus.Output
    ( saveSplit
    ) where


import           Control.Applicative
import qualified Data.Char                 as C
import           Data.List                 (foldl')
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Filesystem.Path.CurrentOS hiding (concat)
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude                   hiding (FilePath)
import Filesystem

import           Phaedrus.Types


saveSplit :: FilePath -> Split -> IO ()
saveSplit dataDir split =
    writeTextFile (makeFileName dataDir split) $ _splitText split

makeFileName :: FilePath -> Split -> FilePath
makeFileName dir Split{..} =
    dir </> FS.fromText (T.intercalate "-" parts) <.> "txt"
    where parts = [ toText' _splitPath
                  , clean _splitId
                  , tshow _splitN
                  ]

tshow :: Show a => a -> T.Text
tshow = T.pack . show

toText' :: FilePath -> T.Text
toText' = either id id . FS.toText

clean :: T.Text -> T.Text
clean = T.filter (\c -> C.isAscii c && C.isAlphaNum c) . T.map char

char :: Char -> Char
char '\x03b1' = 'a'
char '\x03b2' = 'b'
char '\x03b3' = 'g'
char '\x03b4' = 'd'
char '\x03b5' = 'e'
char '\x03b6' = 'z'
char '\x03b7' = 'h'
char '\x03b8' = 'q'
char '\x03b9' = 'i'
char '\x03ba' = 'k'
char '\x03bb' = 'l'
char '\x03bc' = 'm'
char '\x03bd' = 'n'
char '\x03be' = 'c'
char '\x03bf' = 'o'
char '\x03c0' = 'p'
char '\x03c1' = 'r'
char '\x03c2' = 's'
char '\x03c3' = 's'
char '\x03c4' = 't'
char '\x03c5' = 'u'
char '\x03c6' = 'f'
char '\x03c7' = 'x'
char '\x03c8' = 'y'
char '\x03c9' = 'w'
char '\x03dd' = 'v'
char '\x03f2' = 's'
char c        = c

