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
    , splitText
    ) where


import           Control.Lens
import           Data.Text
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)


data Division = Document | Section | Page | Speaking
              deriving (Show, Read, Eq, Enum)

type WindowSize   = Int
type WindowOffset = Int
type WindowSpec   = (WindowSize, WindowOffset)

data Split
        = Split
        { _splitPath :: !FilePath
        , _splitId   :: !Text
        , _splitN    :: !Int
        , _splitText :: !Text
        } deriving (Show)
$(makeLenses ''Split)

