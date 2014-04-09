{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Phaedrus.XML
    ( fileToTextLoc
    , textToTextLoc
    , toTextLoc
    ) where


import           Control.Applicative
import           Control.Error                hiding (just)
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List
import qualified Data.Conduit.List            as CL
import qualified Data.Maybe                   as M
import           Data.Monoid
import           Data.Text
import qualified Data.Text                    as T
import           Data.Text.Read
import           Data.XML.Types
import           Filesystem.Path.CurrentOS
import           Prelude                      hiding (FilePath)
import           Text.XML.Stream.Parse        hiding (content)

import           Phaedrus.Types
import           Phaedrus.XML.Utils


fileToTextLoc :: FilePath -> IO [TextLoc]
fileToTextLoc fp = runResourceT $ parseFile def fp $= toTextLoc fp $$ consume

textToTextLoc :: FilePath -> Text -> IO [TextLoc]
textToTextLoc fp text =  sourceList [text]
                      $= parseText def
                      $= CL.map snd
                      $= toTextLoc fp
                      $$ consume

toTextLoc :: Monad m => FilePath -> Conduit Event m TextLoc
toTextLoc fp = concatMapAccum onEvent (emptyEventState fp)

type TagStack = [Name]

data EventState = ES
                { esFilePath :: !FilePath
                , esTagStack :: !TagStack
                , esTitle    :: !(Maybe Text)
                , esSpeaker  :: !(Maybe Text)
                , esSpeechN  :: !Int
                , esPage     :: !(Maybe Int)
                , esSection  :: !(Maybe Text)
                }

emptyEventState :: FilePath -> EventState
emptyEventState fp = ES (filename fp) [] Nothing Nothing 0 Nothing Nothing

push :: EventState -> Name -> EventState
push es@ES{..} name = es { esTagStack = name : esTagStack }

onEvent :: Event -> EventState -> (EventState, [TextLoc])
onEvent EventBeginDocument             es = (es, [])
onEvent EventEndDocument               es = (es, [])
onEvent (EventBeginDoctype _ _)        es = (es, [])
onEvent EventEndDoctype                es = (es, [])
onEvent (EventInstruction _)           es = (es, [])
onEvent (EventComment _)               es = (es, [])

onEvent (EventBeginElement name attrs) es
    | name == "milestone" = (milestone unit n es', [])
    | otherwise = (es', [])
    where es'  = push es name
          unit = T.concat . M.mapMaybe just <$> lookup "unit" attrs
          n    = T.concat . M.mapMaybe just <$> lookup "n"    attrs
onEvent (EventEndElement n) es@ES { esTagStack=top:ts }
    | top == n = ( es { esTagStack = ts }
                 , [])
    | otherwise = (es, [])
onEvent (EventContent c) es = onText (just c)   es
onEvent (EventCDATA c) es   = onText (emptyt c) es

onText :: Maybe Text -> EventState -> (EventState, [TextLoc])
onText Nothing es = (es, [])
onText (Just t) es@ES{..}
    | "head"    `isTop` esTagStack = (es { esTitle   = t' }, [])
    | "speaker" `isTop` esTagStack = (es { esSpeaker = t' }, [])
    | esTagStack `inContext` "castList" = (es, [])
    | otherwise = (es, M.maybeToList tl)
    where t' =   emptyt t
          tl =   TextLoc
             <$> esTitle
             <*> pure esFilePath
             <*> pure (Speech esSpeaker esSpeechN)
             <*> esPage
             <*> esSection
             <*> pure False
             <*> t'

milestone :: Maybe Text -> Maybe Text -> EventState -> EventState
milestone (Just "speech")  _        es@ES{..} = es { esSpeechN = succ esSpeechN
                                                   , esSpeaker = Nothing
                                                   }
milestone (Just "section") n        es@ES{..} = es { esSection = n }
milestone (Just "page")    (Just n) es@ES{..} = es { esPage = readint n }
milestone (Just "page")    Nothing  es        = es
milestone (Just _)         _        es        = es
milestone Nothing          _        es        = es

readint :: T.Text -> Maybe Int
readint = hush . fmap fst . decimal

inContext :: TagStack -> Name -> Bool
inContext = flip elem

isTop :: Name -> TagStack -> Bool
isTop n (t:ts) = t == n
isTop _ []     = False

isEmpty :: Content -> Bool
isEmpty (ContentText t)   = T.null $ T.strip t
isEmpty (ContentEntity _) = False

content :: Content -> Text
content (ContentText t)   = t
content (ContentEntity t) = "&" <> t <> ";"

just :: Content -> Maybe Text
just = emptyt . T.strip . content

emptyt :: Text -> Maybe Text
emptyt t | T.null t  = Nothing
               | otherwise = Just t

