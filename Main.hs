{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- TODO: group on Division
-- TODO: fabfile
-- TODO: JSON description
-- TODO: Script monad (ErrorT IO)
-- TODO: refactor


module Main where


import           Control.Error
import           Control.Monad
import           Data.Char                 (toLower)
import           Data.Maybe
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
import           Phaedrus.Text.Tokens
import           Phaedrus.Types
import           Phaedrus.XML


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

defaultTrainingSize :: Int
defaultTrainingSize = 100

defaultEvidenceRatio :: Double
defaultEvidenceRatio = 0.5


phaedrus :: PhaedrusOpts -> IO ()

phaedrus PhO{ phoVersion = True } =
    putStrLn $ "phaedrus: version " ++ showVersion version

phaedrus PhO{ phoOutput = Nothing } =
    putStrLn "You must specify an output directory."

phaedrus pho@PhO{..} = do
    createTree dataDir
    files <- filter (`hasExtension` "xml") <$> listDirectory phoDataDir

    putStrLn $ "Saving data files to " ++ encodeString phoOutput'
    eset <- maybe (putStrLn "No evidence file. Skipping." >> return Nothing)
                  (fmap hush . readEvidence)
                  phoEvidenceFile

    splits <- fmap concat . forM files $ \xml -> do
        tlocs' <- fileToTextLoc xml
        let tlocs   = maybe tlocs' (`tagEvidence` tlocs') eset
            tokens  = concatMap tokenizeTextLoc tlocs
            windows = window phoWindow phoOffset tokens
        return . mapMaybe (uncurry (textLocsToSplit phoDivision))
               $ zip [1..] windows

    mapM_ (saveSplit dataDir) splits
    (evidence, nonEvidence) <- makeTrainingSet phoTrainingSize
                                                phoEvidenceRatio
                                                _splitEvidence
                                                splits
    let evidenceDir    = phoOutput' </> "evidence"
        nonEvidenceDir = phoOutput' </> "non-evidence"
    createTree evidenceDir
    createTree nonEvidenceDir

    putStrLn $ "Saving " ++ show (length evidence) ++ " chunks as evidence."
    mapM_ (saveSplit evidenceDir) evidence
    putStrLn $  "Saving " ++ show (length nonEvidence)
             ++ " chunks as non-evidence."
    mapM_ (saveSplit nonEvidenceDir) nonEvidence

    where phoOutput'    = fromMaybe "." phoOutput
          dataDir       = phoOutput' </> "data"


main :: IO ()
main =   decodeString <$> getDataDir
     >>= execParser . opts . (</> "data")
     >>= phaedrus


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
