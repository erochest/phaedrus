{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Phaedrus.Split
    ( window
    , textLocsToSplit
    ) where


import qualified Data.Text as T

import           Phaedrus.Types


window :: WindowSize -> WindowOffset -> [a] -> [[a]]
window _ _ [] = []
window size offset xs = take size xs : window size offset (drop offset xs)

textLocsToSplit :: Division -> Int -> [TextLoc] -> Maybe Split
textLocsToSplit _ _ [] = Nothing
textLocsToSplit d n tls@(tl:_) =
    Just . Split (_tlFile tl) (divisionId d tl) (any _tlEvidence tls) n
         . T.unwords
         $ map _tlText tls

divisionId :: Division -> TextLoc -> T.Text
divisionId Document = _tlTitle
divisionId Section  = _tlSection
divisionId Page     = T.pack . show . _tlPage
divisionId Speaking = undefined

