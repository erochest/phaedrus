{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Phaedrus.Output
    ( saveSplit
    ) where


import           Control.Applicative
import qualified Data.Char                 as C
import           Data.List                 (foldl')
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
    writeTextFile (makeFileName dataDir split) $ _splitText split

makeFileName :: FilePath -> Split -> FilePath
makeFileName dir Split{..} =
    dir </> FS.fromText (T.intercalate "-" parts) <.> "txt"
    where parts = [ toText' _splitPath
                  , clean _splitId
                  , tshow _splitN
                  ]

