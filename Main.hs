{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- TODO: fabfile
-- TODO: JSON description


module Main where


import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char                 (toLower)
import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L
import           Data.Maybe
import           Data.Ord
import qualified Data.Text                 as T
import           Data.Version
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (concat)
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative
import           Paths_phaedrus
import           Prelude                   hiding (FilePath)

import           Phaedrus.Evidence
import           Phaedrus.Output
import           Phaedrus.Split
import           Phaedrus.Text.TfIdf
import           Phaedrus.Text.Tokens
import           Phaedrus.Types
import           Phaedrus.Utils
import           Phaedrus.XML


defaultTrainingSize :: Int
defaultTrainingSize = 100

defaultEvidenceRatio :: Double
defaultEvidenceRatio = 0.5


phaedrus :: PhaedrusOpts -> Phaedrus ()

phaedrus PhO{ phoVersion = True } =
    putStrLn' $ "phaedrus: version " ++ showVersion version

phaedrus PhO{ phoOutput = Nothing } =
    putStrLn' "You must specify an output directory."

phaedrus pho@PhO{..} = do
    createTree' dataDir
    files <- filter (`hasExtension` "xml") <$> ls phoDataDir

    putStrLn' $ "Saving data files to " ++ encodeString phoOutput'
    splits <-  getEvidenceFile phoEvidenceFile
           >>= getSplits phoDivision phoWindow phoOffset files

    mapM_ (saveSplit dataDir) splits

    putStrLn' $ "Saving frequencies file to " ++ encodeString freqFile
    let (corpus, freqs) = processTfIdf
                        $ map (fmap T.toLower . tokenize . _splitText) splits
    saveFrequencies freqFile corpus freqs
    createTree' freqDir
    sequence_ $ zipWith (saveDocumentFrequencies freqDir) splits freqs

    saveStopLists stopDir corpus

    maybe (return ())
          (saveEvidence phoTrainingSize phoEvidenceRatio splits phoOutput')
          phoEvidenceFile

    where phoOutput' = fromMaybe "." phoOutput
          dataDir    = phoOutput' </> "data"
          freqFile   = phoOutput' </> "frequencies.csv"
          freqDir    = phoOutput' </> "freqs"
          stopDir    = phoOutput' </> "stop-lists"


main :: IO ()
main =   decodeString <$> getDataDir
     >>= execParser . opts . (</> "data")
     >>= runPhaedrus . phaedrus


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
    <*> mfileOpt   (  short 'o'
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
    <*> mfileOpt   (  short 'e'
                   <> long "evidence"
                   <> metavar "EVIDENCE-FILE"
                   <> help ( "The optional CSV file listing the evidentiary\
                           \ sections. Each line lists the dialogue\
                           \ title and the section (using the Latin\
                           \ alphabet)."))
    <*> option     (  short 't'
                   <> long "training-size"
                   <> metavar "TRAINING-SIZE"
                   <> value defaultTrainingSize
                   <> help ( "The default size for the training set, including\
                           \ both evidence and non-evidence groups. Default\
                           \ is " ++ show defaultTrainingSize ++ "."))
    <*> option     (  short 'r'
                   <> long "evidence-ratio"
                   <> metavar "EVIDENCE-RATIO"
                   <> value defaultEvidenceRatio
                   <> help ( "The amount of the training set to devote to\
                           \ evidence. For example, if TRAINING-SIZE is 100\
                           \ and EVIDENCE-RATIO is 0.3, this will try to have\
                           \ 30 examples of evidence in the training set.\
                           \ if there's not enough to do this, then the\
                           \ training size is decreased. Default is " ++
                           show defaultEvidenceRatio ++ "."))

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

mfileOpt :: Mod OptionFields (Maybe FilePath) -> Parser (Maybe FilePath)
mfileOpt fields = nullOption (  reader (pure . Just . decodeString)
                             <> value Nothing
                             <> fields)
