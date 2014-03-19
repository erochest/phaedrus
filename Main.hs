{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import qualified Data.Text                 as T
import           Data.Version
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative
import           Paths_phaedrus
import           Prelude                   hiding (FilePath)


data PhaedrusOpts = PhO
                  { phoVersion :: Bool
                  , phoDataDir :: FilePath
                  } deriving (Eq, Show)


phaedrus :: PhaedrusOpts -> IO ()

phaedrus PhO{ phoVersion = True } =
    putStrLn $ "phaedrus: version " ++ showVersion version

phaedrus phOpts = print phOpts


main :: IO ()
main = do
    dataDir <- decodeString <$> getDataDir
    execParser (opts dataDir) >>= phaedrus


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
    <$> switch (  short 'V'
                <> long "version"
                <> help "Display the version information.")
    <*> fileOpt (  short 'd'
                <> long "data-dir"
                <> value dataDir
                <> help (  "The directory for the data (default = "
                        ++ encodeString dataDir ++ ")."))

fileOpt :: Mod OptionFields FilePath -> Parser FilePath
fileOpt fields = nullOption (reader (pure . decodeString) <> fields)
