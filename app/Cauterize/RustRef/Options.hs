module Cauterize.RustRef.Options
  ( runWithOptions
  , RustOpts(..)
  ) where

import Options.Applicative

runWithOptions :: (RustOpts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

data RustOpts = RustOpts { specFile :: FilePath
                         , outputDirectory :: FilePath
                         } deriving (Show)

options :: ParserInfo RustOpts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Process Cauterize schema files."
            )

optParser :: Parser RustOpts
optParser = RustOpts
  <$> strOption
    ( long "spec"
   <> short 's'
   <> metavar "FILE_PATH"
   <> help "Input Cauterize specification file."
    )
  <*> strOption
    ( long "output"
   <> short 'o'
   <> metavar "DIRECTORY_PATH"
   <> help "Output Cauterize directory."
    )
