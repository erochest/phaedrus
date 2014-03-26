{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Phaedrus.Types
    ( Division(..)
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
    ) where


import           Control.Lens
import           Data.Hashable
import qualified Data.HashSet              as S
import           Data.Text
import           Filesystem.Path.CurrentOS
import           GHC.Generics              (Generic)
import           Prelude                   hiding (FilePath)

import           Phaedrus.Text.BetaCode    (BetaCode, toBeta, unBeta)


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

