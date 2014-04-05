{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}


module Phaedrus.Types
    ( Phaedrus(..)
    , runPhaedrus
    , PhaedrusOpts(..)
    , Division(..)
    , WindowSize
    , WindowOffset
    , WindowSpec
    , Split(..)
    , splitPath
    , splitId
    , splitN
    , splitEvidence
    , splitText
    , Speech(..)
    , speaker
    , speechN
    , TextLoc(..)
    , tlTitle
    , tlFile
    , tlSpeech
    , tlPage
    , tlSection
    , tlEvidence
    , tlText
    , EvidencePoint(..)
    , epTitle
    , epSection
    , EvidenceSet
    , BetaCode
    , unBeta
    , toBeta
    , FreqPair(..)
    , freqTotal
    , freqDoc
    , Frequencies
    , Corpus(..)
    , corpusDocuments
    , corpusTypes
    , Document
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Data.Monoid
import           Data.Text
import qualified Data.Vector.Unboxed       as V
import           Filesystem.Path.CurrentOS
import           GHC.Generics              (Generic)
import           Prelude                   hiding (FilePath)

import           Phaedrus.Text.BetaCode    (BetaCode, toBeta, unBeta)


newtype Phaedrus a = Phaedrus { unPhaedrus :: Script a }
                     deriving (Functor, Applicative, Monad, MonadIO)


data PhaedrusOpts
        = PhO
        { phoVersion       :: !Bool
        , phoDataDir       :: !FilePath
        , phoOutput        :: !(Maybe FilePath)
        , phoDivision      :: !Division
        , phoWindow        :: !WindowSize
        , phoOffset        :: !WindowOffset
        , phoEvidenceFile  :: !(Maybe FilePath)
        , phoTrainingSize  :: !Int
        , phoEvidenceRatio :: !Double
        } deriving (Eq, Show)


data Hole = Hole

runPhaedrus :: Phaedrus a -> IO a
runPhaedrus = runScript . unPhaedrus

data Division = Document | Section | Page | Speaking
              deriving (Show, Read, Eq, Enum)

type WindowSize   = Int
type WindowOffset = Int
type WindowSpec   = (WindowSize, WindowOffset)

data Split
        = Split
        { _splitPath     :: !FilePath
        , _splitId       :: !Text
        , _splitEvidence :: !Bool
        , _splitN        :: !Int
        , _splitText     :: !Text
        } deriving (Show)
$(makeLenses ''Split)

data Speech
        = Speech
        { _speaker :: !(Maybe Text)
        , _speechN :: !Int
        } deriving (Show)
$(makeLenses ''Speech)

data TextLoc
        = TextLoc
        { _tlTitle    :: !Text
        , _tlFile     :: !FilePath
        , _tlSpeech   :: !Speech
        , _tlPage     :: !Int
        , _tlSection  :: !Text
        , _tlEvidence :: !Bool
        , _tlText     :: !Text
        } deriving (Show)
$(makeLenses ''TextLoc)

data EvidencePoint
        = Evidence
        { _epTitle   :: !BetaCode
        , _epSection :: !Text
        } deriving (Eq, Show, Generic)
$(makeLenses ''EvidencePoint)

instance Hashable EvidencePoint

type EvidenceSet = S.HashSet EvidencePoint

type Frequencies a = M.HashMap a Int

data FreqPair = FreqPair
              { _freqTotal :: !Int
              , _freqDoc   :: !Int
              } deriving (Show)
$(makeLenses ''FreqPair)

instance Monoid FreqPair where
    mempty = FreqPair 0 0
    a `mappend` b = FreqPair (_freqTotal a + _freqTotal b)
                             (_freqDoc   a + _freqDoc   b)

data Corpus a = Corpus
              { _corpusDocuments :: !Int
              , _corpusTypes     :: !(M.HashMap a FreqPair)
              }
$(makeLenses ''Corpus)

type Document a = [a]

