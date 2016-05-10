{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Cauterize.RustRef.Generate
import           Cauterize.RustRef.Options
import           Cauterize.RustRef.Util
import           Data.Text
import           Data.Text.IO               (writeFile)
import           Prelude                    hiding (writeFile)
import           System.Directory
import           System.FilePath.Posix

import qualified Cauterize.Specification    as S

main :: IO ()
main = runWithOptions caut2rust

caut2rust :: RustOpts -> IO ()
caut2rust RustOpts { specFile = sf, outputDirectory = od } = createGuard od $ do
  spec <- loadSpec sf
  let baseName = unpack $ S.specName spec
  generateDynamicFiles od baseName spec
  where
    loadSpec :: FilePath -> IO S.Specification
    loadSpec p = do
      s <- S.parseSpecificationFromFile p
      case s of
        Left e -> error $ show e
        Right s' -> return s'

createGuard :: FilePath -> IO a -> IO a
createGuard out go = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out
  if fe
    then error $ "Error: " ++ out ++ " is a file."
    else if de
          then go
          else createDirectory out >> go

generateDynamicFiles :: FilePath -> String -> S.Specification -> IO ()
generateDynamicFiles path baseName spec = do
  writeFile fullName (spec2rust spec)
  where
    fullName = combine path (baseName ++ ".rs")
