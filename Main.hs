{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- TODO: evidence
-- TODO: no-evidence
-- TODO: fabfile
-- TODO: JSON description


module Main where


import           Control.Monad
import           Data.Char                 (toLower)
import qualified Data.Text                 as T
import           Data.Version
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (concat)
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative
import           Paths_phaedrus
import           Prelude                   hiding (FilePath)

import Phaedrus.Output
import           Phaedrus.Split
import           Phaedrus.Types


data PhaedrusOpts
        = PhO
        { phoVersion  :: !Bool
        , phoDataDir  :: !FilePath
        , phoOutput   :: !FilePath
        , phoDivision :: !Division
        , phoWindow   :: !WindowSize
        , phoOffset   :: !WindowOffset
        } deriving (Eq, Show)


phaedrus :: PhaedrusOpts -> IO ()

phaedrus PhO{ phoVersion = True } =
    putStrLn $ "phaedrus: version " ++ showVersion version

phaedrus pho@PhO{..} = do
    print pho
    createTree phoOutput
    mapM_ (saveSplit phoOutput)
        =<< fmap concat . mapM (splitFile phoDivision window)
        =<< filter (`hasExtension` "xml") <$> listDirectory phoDataDir

    where window = (phoWindow, phoOffset)


main :: IO ()
main =
    phaedrus =<< execParser . opts . (</> "data") =<< decodeString <$> getDataDir


opts :: FilePath -> ParserInfo PhaedrusOpts
opts dataDir =
    info (helper <*> phopts dataDir)
         (  fullDesc
         <> progDesc "Generate a training/test/dataset from the Dialogues."
         <> header (  "phaedrus v" ++ showVersion version
                   ++ ": dialogue dataset generator")
         )

phopts :: FilePath -> Parser PhaedrusOpts
phopts dataDir =
        PhO
    <$> switch     (  short 'V'
                   <> long "version"
                   <> help "Display the version information.")
    <*> fileOpt    (  short 'd'
                   <> long "data-dir"
                   <> value dataDir
                   <> help (  "The directory for the data (default = "
                           ++ encodeString dataDir ++ ")."))
    <*> fileOpt    (  short 'o'
                   <> long "output-dir"
                   <> help "The output directory.")
    <*> nullOption (  short 'D'
                   <> long "division"
                   <> reader divisionReader
                   <> value Document
                   <> help (  "How to divide the document. One of [D]ocument,\
                           \ [Se]ction, [P]age, or [Sp]eaking. (The brackets\
                           \ represent an abbreviation for that option.\
                           \ Default is 'Document.')"))
    <*> option     (  short 'w'
                   <> long "window"
                   <> value 500
                   <> help (  "The size of the window for the output chunks "
                           ++ "(default = 500)."))
    <*> option     (  short 'O'
                   <> long "offset"
                   <> value 250
                   <> help (  "The offset between the beginning of each window "
                           ++ "(default = 250)."))

divisionReader :: String -> ReadM Division
divisionReader "" = readerError "Invalid division value: \"\""
divisionReader division
    | div'      == 'd'                            = return Document
    | div'      == 'p'                            = return Page
    | division  == "section"  || division == "se" = return Section
    | division  == "speaking" || division == "sp" = return Speaking
    where division' = map toLower division
          div'      = head division'

fileOpt :: Mod OptionFields FilePath -> Parser FilePath
fileOpt fields = nullOption (reader (pure . decodeString) <> fields)
