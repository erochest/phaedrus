{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Phaedrus.Text.TfIdf
    ( emptyCorpus
    , buildCorpus
    , frequencies
    , tf
    , idf
    , tfIdf
    , processTfIdf
    ) where


import           Control.Applicative
import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Data.Maybe

import           Phaedrus.Types


emptyCorpus :: Corpus a
emptyCorpus = Corpus 0 M.empty

buildCorpus :: (Hashable a, Eq a) => [Document a] -> Corpus a
buildCorpus = L.foldl' foldDocument emptyCorpus

foldDocument :: (Hashable a, Eq a) => Corpus a -> Document a -> Corpus a
foldDocument c = foldDocumentFreqs c . frequencies

frequencies :: (Hashable a, Eq a) => Document a -> M.HashMap a Int
frequencies = L.foldl' f M.empty
    where f m k = M.insertWith (+) k 1 m

tf :: Int -> Int -> Double
tf maxf f = 0.5 + (0.5 * fromIntegral f) / fromIntegral maxf

idf :: Corpus a -> M.HashMap a Double
idf Corpus{..} = M.map (idf' _corpusDocuments) _corpusTypes
    where idf' n FreqPair{..} =
            log (fromIntegral n / (1.0 + fromIntegral _freqTotal))

tfIdf :: (Hashable a, Eq a)
      => M.HashMap a Double -> M.HashMap a Double -> M.HashMap a Double
tfIdf idf = runIdentity . M.traverseWithKey tfIdf'
    where tfIdf' t f = pure $ f * fromMaybe 0.0 (M.lookup t idf)

processTfIdf :: (Hashable a, Eq a)
             => [Document a] -> (Corpus a, [M.HashMap a (Int, Double)])
processTfIdf docs = (c, map docp docs)
    where c      = buildCorpus docs
          idfc   = idf c
          docp d = let f = frequencies d
                   in  mergeFreqs f . tfIdf idfc $ tfDoc f


mergeFreqs :: (Hashable a, Eq a)
           => M.HashMap a Int -> M.HashMap a Double -> M.HashMap a (Int, Double)
mergeFreqs r s = M.intersectionWith (,) r s

tfDoc :: (Hashable a, Eq a) => M.HashMap a Int -> M.HashMap a Double
tfDoc m = M.map (tf maxf) m
    where maxf = M.foldl' max 0 m

foldDocumentFreqs :: (Hashable a, Eq a)
                  => Corpus a -> M.HashMap a Int -> Corpus a
foldDocumentFreqs c f = c & corpusDocuments +~ 1
                          & corpusTypes     %~ updateTypes
    where updateTypes ts = L.foldl' update1 ts $ M.toList f
          update1 ts (t, f) = flip (M.insert t) ts
                            . maybe (singleton f) (addDocumentFreq f)
                            $ M.lookup t ts

singleton :: Int -> FreqPair
singleton f = FreqPair f 1

addDocumentFreq :: Int -> FreqPair -> FreqPair
addDocumentFreq f fp = FreqPair (_freqTotal fp + f) (_freqDoc fp + 1)


