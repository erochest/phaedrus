{-# LANGUAGE OverloadedStrings #-}


module Phaedrus.Text.Tokens
    ( tokenize
    , tokenizeBreaks
    , tokenizeTextLoc
    ) where


import           Control.Lens
import qualified Data.Char      as C
import qualified Data.Text      as T
import           Data.Text.ICU

import           Phaedrus.Types


grc :: LocaleName
grc = Locale "grc"


tokenize :: T.Text -> [T.Text]
tokenize = map brkBreak . tokenizeBreaks

tokenizeBreaks :: T.Text -> [Break Word]
tokenizeBreaks = filter (not . T.all C.isSpace . brkBreak)
               . filter ((/= Uncategorized) . brkStatus)
               . breaks (breakWord grc)

tokenizeTextLoc :: TextLoc -> [TextLoc]
tokenizeTextLoc tl =
    over traverse (flip (set tlText) tl) . tokenize $ _tlText tl

