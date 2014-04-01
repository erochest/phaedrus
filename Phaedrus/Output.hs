{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Phaedrus.Output
    ( saveSplit
    , saveFrequencies
    , saveDocumentFrequencies
    ) where


import           Control.Applicative
import qualified Data.Char                 as C
import qualified Data.HashMap.Strict       as M
import           Data.List                 (foldl', sortBy)
import           Data.Monoid
import           Data.Ord
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (concat)
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude                   hiding (FilePath)

import           Phaedrus.Text.BetaCode    (clean)
import           Phaedrus.Types
import           Phaedrus.Utils


saveSplit :: FilePath -> Split -> IO ()
saveSplit dataDir split =
    writeTextFile (makeFileName dataDir split "txt") $ _splitText split

makeFileName :: FilePath -> Split -> T.Text -> FilePath
makeFileName dir Split{..} ext =
    dir </> FS.fromText (T.intercalate "-" parts) <.> ext
    where parts = [ toText' (basename _splitPath)
                  , clean _splitId
                  , tshow _splitN
                  ]

saveFrequencies :: FilePath
                -> Corpus T.Text
                -> [M.HashMap T.Text (Int, Double)]
                -> IO ()
saveFrequencies filename c docFreqs =
    writeTextFile filename
        . T.concat
        . (header:)
        . map row
        . M.toList
        $ _corpusTypes c
    where maxf f token = foldl' max 0
                       $ map (f . M.lookupDefault (0,0) token) docFreqs
          row (token, docFreq@FreqPair{..}) =
                T.intercalate "," [ token
                                  , tshow _freqDoc
                                  , tshow _freqTotal
                                  , tshow (maxf fst token)
                                  , tshow (maxf snd token)
                                  ]
                <> "\n"
          header = "Token,Document Count,Total Count,\
                   \Max Single-Document Frequency,Max Single-Document TF-IDF\n"

saveDocumentFrequencies :: FilePath
                        -> Split
                        -> M.HashMap T.Text (Int, Double)
                        -> IO ()
saveDocumentFrequencies dir split freqs =
    writeTextFile (makeFileName dir split "csv")
        . T.concat
        . (header:)
        . map row
        . sortBy (comparing snd)
        $ M.toList freqs
    where row (token, (raw, scaled)) =
            T.intercalate "," [token, tshow raw, tshow scaled] <> "\n"
          header = "Token,Raw Frequency,TF-IDF\n"

