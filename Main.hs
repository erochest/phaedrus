{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import qualified Data.Text           as T
import           Data.Version
import           Options.Applicative
import           Paths_phaedrus


data PhaedrusOpts = PhO
                  { phoVersion :: Bool
                  } deriving (Eq, Show)


phaedrus :: PhaedrusOpts -> IO ()
phaedrus PhO{ phoVersion = True } = do
    putStrLn $ "phaedrus: version " ++ showVersion version


main :: IO ()
main = execParser opts >>= phaedrus


opts :: ParserInfo PhaedrusOpts
opts = info (helper <*> phopts)
            (  fullDesc
            <> progDesc "Generate a training/test/dataset from the Dialogues."
            <> header (  "phaedrus v" ++ showVersion version
                      ++ ": dialogue dataset generator")
            )

phopts :: Parser PhaedrusOpts
phopts =   PhO
       <$> switch (  short 'V'
                  <> long "version"
                  <> help "Display the version information.")
