{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Phaedrus.Evidence
    ( readEvidence
    , tagEvidence
    , makeTrainingSet
    ) where


import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy      as BS
import           Data.Csv
import qualified Data.HashSet              as S
import qualified Data.List                 as L
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Debug.Trace
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (decode)
import           Prelude                   hiding (FilePath, readFile)

import           Phaedrus.Types
import           Phaedrus.Utils

import Debug.Trace


readEvidence :: FilePath -> IO (Either String EvidenceSet)
readEvidence fp =   fmap (S.fromList . V.toList)
                .   decode NoHeader
                .   BS.fromStrict
                <$> readFile fp

tagEvidence :: EvidenceSet -> [TextLoc] -> [TextLoc]
tagEvidence eset = map (flipEvidence eset)

textLocEvidence :: TextLoc -> EvidencePoint
textLocEvidence TextLoc{..} = Evidence (toBeta _tlTitle) _tlSection

isEvidence :: EvidenceSet -> TextLoc -> Bool
isEvidence eset = (`S.member` eset) . textLocEvidence

flipEvidence :: EvidenceSet -> TextLoc -> TextLoc
flipEvidence eset tl = tl { _tlEvidence = isEvidence eset tl }


instance FromRecord EvidencePoint where
    parseRecord v
        | V.length v == 2 =   Evidence
                          <$> fmap (toBeta . T.strip) (v .! 0)
                          <*> fmap T.strip (v .! 1)
        | otherwise       = mzero

makeTrainingSet :: Int -> Double -> (a -> Bool) -> [a] -> IO ([a], [a])
makeTrainingSet size ratio p xs = do
    ts <- take targetTsize <$> shuffle ts'
    let targetSize = min size $ backr ratio ts
        fsize      = targetSize - length ts
    (ts,) . take fsize <$> shuffle fs'
    where (ts', fs')  = L.partition p xs
          targetTsize = rationl ratio xs

          rationl :: Double -> [a] -> Int
          rationl r = truncate . (* r) . fromIntegral . length
          backr :: Double -> [a] -> Int
          backr r = truncate . (/ r) . fromIntegral . length

traceIO' :: Show a => a -> IO ()
traceIO' = traceIO . show

