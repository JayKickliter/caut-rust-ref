{-# LANGUAGE TemplateHaskell #-}
module Cauterize.RustRef.Static
  ( allFiles
  ) where

import qualified Data.ByteString as B
import           Data.FileEmbed

allFiles :: [(FilePath, B.ByteString)]
allFiles =
  [ ("cauterize/Cargo.toml", $(embedFile "static/cauterize/Cargo.toml"))
  , ("cauterize/src/cauterize.rs", $(embedFile "static/cauterize/src/cauterize.rs"))
  , ("cauterize/src/error.rs", $(embedFile "static/cauterize/src/error.rs"))
  , ("cauterize/src/lib.rs", $(embedFile "static/cauterize/src/lib.rs"))
  , ("cauterize/src/range.rs", $(embedFile "static/cauterize/src/range.rs"))
  , ("cauterize/src/vector.rs", $(embedFile "static/cauterize/src/vector.rs"))
  ]
