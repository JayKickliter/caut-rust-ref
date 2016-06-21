{-# LANGUAGE TemplateHaskell #-}
module Cauterize.RustRef.Static
  ( allFiles
  ) where

import qualified Data.ByteString as B
import           Data.FileEmbed


cargoDotToml :: (FilePath, B.ByteString)
cargoDotToml = ("cauterize/Cargo.toml", $(embedFile "static/cauterize/Cargo.toml"))

cauterizeDotRs :: (FilePath, B.ByteString)
cauterizeDotRs = ("cauterize/src/cauterize.rs", $(embedFile "static/cauterize/src/cauterize.rs"))

allFiles :: [(FilePath, B.ByteString)]
allFiles =
  [ cauterizeDotRs
  , cargoDotToml
  ]
