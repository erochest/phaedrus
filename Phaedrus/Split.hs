{-# LANGUAGE OverloadedStrings #-}


module Phaedrus.Split
    ( window
    , textLocsToSplit
    , divideTextLocs
    , getSplits
    ) where


import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.List                 as L
import           Data.Maybe                (fromMaybe, mapMaybe)
import           Data.Ord
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS hiding (concat)
import           Prelude                   hiding (FilePath)

import           Phaedrus.Evidence
import           Phaedrus.Text.Tokens
import           Phaedrus.Types
import           Phaedrus.XML


window :: WindowSize -> WindowOffset -> [a] -> [[a]]
window _ _ [] = []
window size offset xs = take size xs : window size offset (drop offset xs)

textLocsToSplit :: Division -> Int -> [TextLoc] -> Maybe Split
textLocsToSplit _ _ [] = Nothing
textLocsToSplit d n tls@(tl:_) =
    Just . Split (_tlFile tl) (divId d tl) (any _tlEvidence tls) n
         . T.unwords
         $ map _tlText tls

divideTextLocs :: Division -> [TextLoc] -> [[TextLoc]]
divideTextLocs div = L.groupBy (divcmp div) . L.sortBy (divon div)

divId :: Division -> TextLoc -> T.Text
divId Document = _tlTitle
divId Section  = _tlSection
divId Page     = T.pack . show . _tlPage
divId Speaking = fromMaybe "unknown" . _speaker . _tlSpeech

divon :: Division -> TextLoc -> TextLoc -> Ordering
divon Document = comparing _tlTitle
divon Section  = comparing _tlSection
divon Page     = comparing _tlPage
divon Speaking = comparing (_speaker . _tlSpeech)

divcmp :: Division -> TextLoc -> TextLoc -> Bool
divcmp div a b = divon div a b == EQ

getSplits :: Division
          -> WindowSize
          -> WindowOffset
          -> [FilePath]
          -> Maybe EvidenceSet
          -> Phaedrus [Split]
getSplits division windowSize offset files eset =
    fmap concat . forM files $ \xml -> do
        tlocs' <- liftIO $ fileToTextLoc xml
        return . mapMaybe (uncurry (textLocsToSplit division))
                . zip [1..]
                . concatMap ( window windowSize offset
                            . concatMap tokenizeTextLoc)
                . divideTextLocs division
                $ maybe tlocs' (`tagEvidence` tlocs') eset

