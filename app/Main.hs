{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Cauterize.RustRef.Generate
import           Cauterize.RustRef.Options
import           Cauterize.RustRef.Tester
import           Cauterize.RustRef.Static
import qualified Data.ByteString            as B
import           Data.Text
import           Data.Text.IO               (writeFile)
import           Prelude                    hiding (writeFile)
import           System.Directory
import           System.FilePath.Posix

import qualified Cauterize.Specification    as S

main :: IO ()
main = runWithOptions caut2rust

caut2rust :: RustOpts -> IO ()
caut2rust RustOpts {..} = do
  createGuard $ outputDirectory </> "src"
  createGuard $ outputDirectory </> "bin"
  createGuard $ outputDirectory </> "cauterize" </> "src"
  spec <- loadSpec specFile
  let baseName = unpack $ S.specName spec
  copyStaticFilesTo outputDirectory
  generateDynamicFiles outputDirectory baseName spec
  where
    loadSpec :: FilePath -> IO S.Specification
    loadSpec p = do
      s <- S.parseSpecificationFromFile p
      case s of
        Left e -> error $ show e
        Right s' -> return s'

createGuard :: FilePath -> IO ()
createGuard out = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out
  if fe
    then error $ "Error: " ++ out ++ " is a file."
    else if de
         then return ()
         else createDirectoryIfMissing True out

copyStaticFilesTo :: FilePath -> IO ()
copyStaticFilesTo path = mapM_ go allFiles
  where
    go (p, d) = B.writeFile (path `combine` p) d

generateDynamicFiles :: FilePath -> String -> S.Specification -> IO ()
generateDynamicFiles path baseName spec = do
  writeFile fullName (genRust spec)
  writeFile manifestName (genManifest spec)
  writeFile testerName (genTester spec)
  where
    srcDir = combine path "src"
    binDir = combine path "bin"
    fullName = combine srcDir (baseName ++ ".rs")
    manifestName = combine path "Cargo.toml"
    testerName = combine binDir "tester.rs"
