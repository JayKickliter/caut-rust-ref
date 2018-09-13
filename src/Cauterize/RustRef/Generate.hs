{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Cauterize.RustRef.Generate
       ( genRust
       , genManifest
       ) where

import qualified Cauterize.CommonTypes   as C
import qualified Cauterize.Hash          as H
import           Cauterize.RustRef.Util
import qualified Cauterize.Specification as S
import           Data.Maybe
import qualified Data.Text               as T
import           Text.PrettyPrint.Leijen hiding (indent)
import qualified Text.PrettyPrint.Leijen as L


----------------------
-- Helper functions --
----------------------

-- | Rust attribute tags
data Attribute = Debug
               | Default
               | PartialEq
               deriving(Show)
instance Pretty Attribute where
  pretty Debug     = s "Debug"
  pretty Default   = s "Default"
  pretty PartialEq = s "PartialEq"


s :: String -> Doc
s = string

t :: T.Text -> Doc
t = s . T.unpack

indent :: Doc -> Doc
indent = L.indent 4

renderDoc :: Doc -> String
renderDoc d = displayS (renderPretty 1.0 200 d) ""

genTypeName :: C.Identifier -> Doc
genTypeName n = s . cautTypeToRustType $ n

genFieldName :: C.Identifier -> Doc
genFieldName = s . cautFieldToRustField

genUnsafe :: Doc -> Doc
genUnsafe d = s "unsafe" <+> braces (space <> d <> space)

genTry :: Doc -> Doc
genTry d = d <> s "?"

genOk :: Doc -> Doc
genOk d = s "Ok" <> parens d

genRepr :: Doc -> Doc
genRepr r = s "#[repr" <> parens r <> rbracket

genTagTypeName :: C.Tag -> Doc
genTagTypeName = s . cautTagToRustType

genUnit :: Doc
genUnit = s "()"

genMatch :: Doc -> [(Doc,Doc)] -> Doc
genMatch a bs = s "match" <+> a <+> genBlock exprs
  where
    matchArm (l,r) = fill 2 l <+> s "=>" <+> r <> comma
    exprs = vcat (map matchArm bs)

genAmp :: Doc
genAmp = s "&"

genRef :: Doc -> Doc
genRef a = genAmp <> a

genStructFieldName :: C.Identifier -> Doc
genStructFieldName = t . C.unIdentifier

genBlock :: Doc -> Doc
genBlock a = braces
             (linebreak
             <> indent a
             <> linebreak
             )

genFor :: Doc -> Doc -> Doc
genFor a b = s "for" <+> a <+> genBlock b

genIf :: Doc -> Doc -> Doc
genIf a b = s "if" <+> a <+> genBlock b

(<::>) :: Doc -> Doc -> Doc
(<::>) a b = a <> s "::" <> b

genSome :: Doc -> Doc
genSome a = s "Some" <> parens a

genTryDecode :: Doc -> Doc
genTryDecode a = genTry (a <> s "::decode(ctx)")

genTryEncode :: Doc -> Doc
genTryEncode a = genTry (a <> s ".encode(ctx)")

genComment :: Doc -> Doc
genComment d = s "//" <+> d

genUninitialized :: Doc
genUninitialized = genUnsafe $ s "mem::uninitialized()"

genPrimTypeName :: C.Prim -> Doc
genPrimTypeName = t . cautPrimToRustPrim

genNewType :: Doc -> Bool ->  Doc-> Doc
genNewType nm pub tp  = vcat
  [ s "#[derive(Debug, Clone, PartialEq)]"
  , s "pub struct" <+> nm <>  parens (visibility <> tp) <>  semi
  ]
  where
    visibility | pub == True = s "pub "
               | otherwise   = empty

genEnum :: Doc -> [Doc] -> Doc
genEnum nm fields = vcat
  [ s "#[derive(Debug, Clone, PartialEq)]"
  , s "pub enum" <+> nm <+> lbrace
  , indent (vcat fields)
  , rbrace
  ]

genStruct :: Doc -> [Doc] -> Doc
genStruct nm fields = vcat
  [ s "#[derive(Debug, Clone, PartialEq)]"
  , s "pub struct" <+> nm <+> lbrace
  , indent (vcat fields)
  , rbrace
  ]

intercalate :: Doc -> [Doc] -> Doc
intercalate seperator elems = cat (punctuate seperator elems)

genFingerprint :: H.Hash -> Doc
genFingerprint f = brackets (intercalate comma bytes)
  where
    bytes = map (s . showByte) (H.hashToBytes f)

-- genDerive :: [Attribute] -> Doc
-- genDerive attrs = s "#[derive" <> parens (intercalate comma (map pretty attrs)) <> rbracket


---------------------------
-- Cargo.toml generation --
---------------------------

genManifest :: S.Specification -> T.Text
genManifest spec =
  T.pack . renderDoc
  $ vcat [ s "[package]"
         , s "name =" <+> dquotes specName
         , s "version = \"0.2.0\"" -- TODO: the following does not work due to cargo's semver requirements: dquotes specVersion
         , s "authors = [\"author\"]"
         , empty
         , s "[lib]"
         , s "name =" <+> dquotes specName
         , s "path =" <+> dquotes (s "src/" <> specName <> s ".rs")
         , s "# See https://github.com/rust-lang/rust/issues/21246"
         , s "doctest = false"
         , empty
         , s "[features]"
         , s "default = [\"std\"]"
         , s "std = [\"byteorder\"]"
         , empty
         , s "[dependencies]"
         , s "byteorder = { version = \"1\", optional = true }"
         , empty
         , s "[[bin]]"
         , s "required-features = [\"std\"]"
         , s "name = \"tester\""
         , s "path = \"bin/tester.rs\""
         , empty
         , s "[target.'cfg(feature = \"std\")'.dependencies]"
         , s "cauterize = { path = \"cauterize\" }"
         , empty
         , s "[target.'cfg(not(feature = \"std\"))'.dependencies]"
         , s "cauterize = { path = \"cauterize\", default-features = false }"
         , empty
         ]
  where
    specName = t $ S.specName spec
    -- specVersion = t $ S.specVersion spec


-----------------------
-- Source generation --
-----------------------

genRust :: S.Specification -> T.Text
genRust = T.pack . genSource

genSource :: S.Specification -> String
genSource S.Specification {..} = renderDoc $ vcat $ punctuate empty
  [ s "#![allow(dead_code, unused_variables, unused_imports)]"
  , s "#![no_std]"
  , empty
  , s "extern crate core as std;"
  , s "#[macro_use]"
  , s "pub extern crate cauterize;"
  , s "use self::cauterize::{Primitive, Error, Encoder, Decoder, Cauterize, Range, Vector};"
  , s "use std::mem;"
  , empty
  , s "pub static SPEC_NAME: &'static str =" <+> dquotes (t specName) <> semi
  , s "pub const SPEC_FINGERPRINT: [u8;20] =" <+> genFingerprint specFingerprint <> semi
  , s "pub const SPEC_MIN_SIZE: usize =" <+> s (show (C.sizeMin specSize)) <> semi
  , s "pub const SPEC_MAX_SIZE: usize =" <+> s (show (C.sizeMax specSize)) <> semi
  , empty
  , vcat [  genType tp <> linebreak
         <> empty <> linebreak
         <> genCautImpl tp <> linebreak
         | tp <- specTypes
         ]
  , empty
  ]


---------------------
-- Type generation --
---------------------

genType :: S.Type -> Doc
genType S.Type {typeDesc = S.Range {..}, ..} =
  s "impl_range!" <> parens (name <> comma <> primType <> comma <> tagType <> comma <> offset <> comma <> len) <> semi
  where
    name     = genTypeName typeName
    primType = genPrimTypeName rangePrim
    tagType  = genTagTypeName rangeTag
    offset   = s . show $ rangeOffset
    len      = s . show $ rangeLength

genType tp@S.Type {typeDesc = S.Combination {..}, ..} =
  genStruct name fields
  where
    name   = genTypeName typeName
    fields = genFields tp

genType tp@S.Type {typeDesc = S.Record {..}, ..} =
  genStruct name fields
  where
    name   = genTypeName typeName
    fields = genFields tp

genType tp@S.Type {typeDesc = S.Enumeration {..}, ..} =
  vcat [ genRepr tagType
       , genEnum name fields
       ]
  where
    name    = genTypeName typeName
    fields  = genFields tp
    tagType = genTagTypeName  enumerationTag

genType S.Type {typeDesc = S.Array {..}, ..} =
  s "impl_array!" <> parens (name <> comma <> elType <> comma <> sz) <> semi
  where
    name   = genTypeName typeName
    elType = genTypeName arrayRef
    sz     = s $ show arrayLength

genType S.Type {typeDesc = S.Vector {..}, ..} =
  s "impl_vector!" <> parens (name <> comma <> elType <> comma <> capacity) <> semi
  where
    name     = genTypeName typeName
    elType   = genTypeName vectorRef
    capacity = s . show $ vectorLength

genType tp@S.Type {typeDesc = S.Union {..}, ..} =
  genEnum name fields
  where
    name = genTypeName typeName
    fields = genFields tp

genType S.Type {typeDesc = S.Synonym {..}, ..} =
  genNewType name True st
  where
    name = genTypeName typeName
    st = genTypeName synonymRef

genStructField :: S.Field -> Doc
genStructField (S.DataField n i r) =
  s "pub" <+> nm <> colon <+> fieldType <> comma <+> genComment idx
  where
    nm        = t . C.unIdentifier $  n
    fieldType = genTypeName r
    idx       = s . show $ i
genStructField (S.EmptyField _ _) = empty

genEnumField :: S.Field -> Doc
genEnumField (S.DataField n i r) =
  nm <> parens fieldType <> comma <+> genComment idx
  where
    nm        = t . titleCase . C.unIdentifier $  n
    fieldType = genTypeName r
    idx       = s . show $ i
genEnumField (S.EmptyField n i) =
  nm <> comma <+> genComment idx
  where
    nm        = t . titleCase . C.unIdentifier $  n
    idx       = s . show $ i

genOption :: Maybe Doc -> Doc
genOption ot = s "Option" <> angles a
  where
   a = fromMaybe (s "()") ot

genComboStructField :: S.Field -> Doc
genComboStructField (S.DataField n i r) =
  s "pub" <+> nm <> colon <+> genOption fieldType <> comma <+> genComment idx
  where
    nm        = t . C.unIdentifier $ n
    fieldType = Just $ genTypeName r
    idx       = s . show $ i
genComboStructField (S.EmptyField n i) =
  s "pub" <+> nm <> colon <+> genOption Nothing <> comma <+> genComment idx
  where
    nm  = t . C.unIdentifier $ n
    idx = s . show $ i

genEnumerationEnumField :: S.EnumVal -> Doc
genEnumerationEnumField (S.EnumVal n i) = nm <> space <> equals <>  space <> idx <> comma
  where
    nm  = s . T.unpack . titleCase . C.unIdentifier $ n
    idx = s . show $ i

genFields :: S.Type -> [Doc]
genFields S.Type {S.typeDesc = td} =
  case td of
    S.Record fs          -> fmap genStructField fs
    S.Union  fs _        -> fmap genEnumField fs
    S.Enumeration  evs _ -> fmap genEnumerationEnumField evs
    S.Combination fs _   -> fmap genComboStructField fs
    _                    -> error "How did I get here?"


---------------------------------
-- Cauterize `impl` generation --
---------------------------------

genCautImpl :: S.Type -> Doc
genCautImpl tp@S.Type {..} =
  vcat [ s "impl Cauterize for" <+> genTypeName typeName <+> lbrace
       , indent $ s "const FINGERPRINT: [u8;20] =" <+> genFingerprint typeFingerprint <> semi
       , indent $ s "const SIZE_MIN: usize =" <+> (s . show . C.sizeMin $ typeSize) <> semi
       , indent $ s "const SIZE_MAX: usize =" <+> (s . show . C.sizeMax $ typeSize) <> semi
       , empty
       , indent $ genEncode tp
       , empty
       , indent $ genDecode tp
       , rbrace
       ]

genEncode :: S.Type -> Doc
genEncode tp =
  s "fn encode(&self, ctx: &mut Encoder) -> Result<(), Error>" <+> genBlock
  (genEncodeInner tp
  )

genEncodeInner :: S.Type -> Doc
genEncodeInner S.Type {typeDesc = S.Synonym {}, ..} =
  vcat [ s "let &" <> genTypeName typeName <> parens (s "ref inner") <+> s "= self;"
       , genTryEncode (s "inner") <> semi
       , genOk (parens empty)
       ]

genEncodeInner S.Type {typeDesc = S.Range {..}, ..} =
  vcat [ s "let tag =" <+> parens (s "self.0 -" <+> offset) <+> s "as" <+> tagType <> semi
       , genOk . genTryEncode $ s "tag"
       ]
  where
    offset  = s . show $ rangeOffset
    tagType = genTagTypeName rangeTag

genEncodeInner S.Type {typeDesc = S.Array {}} =
  vcat [ s "let ref elems = self.0;"
       , genFor (s "elem in elems.iter()") (s "elem.encode(ctx)?;")
       , genOk genUnit
       ]

genEncodeInner S.Type {typeDesc = S.Vector {..}} = vcat
  [ genIf (s "self.len >" <+> maxLen)
          (s "return Err(Error::ElementCount);")
  , genTryEncode (parens (s "self.len as" <+> tagType)) <> semi
  , genFor (s "i in 0..self.len")
           (genTryEncode (s "self.elems[i]"))
  , genOk genUnit
  ]
  where
    tagType = genTagTypeName vectorTag
    maxLen = s . show $ vectorLength

genEncodeInner S.Type {typeDesc = S.Enumeration {..}} =
  vcat [ s "let tag " <+> s "= self.clone() as " <> tagType <> semi
       , s "tag.encode(ctx)?;"
       , genOk genUnit
       ]
  where
    tagType = genTagTypeName enumerationTag

genEncodeInner S.Type {typeDesc = S.Record {..}} =
  vcat (map genEncodeRecordField recordFields)
  <$$> genOk genUnit
  where
    genEncodeRecordField S.DataField {..} =
      genTryEncode (s "self." <> genStructFieldName fieldName) <> semi
    genEncodeRecordField S.EmptyField {} =
      error "A record shound not have an empty field"

genEncodeInner S.Type{typeDesc = S.Combination {..}, ..} =
  vcat [ s "let mut tag:" <+> tagType <+> s "= 0;"
       , vcat $ map encBitField combinationFields
       , genTryEncode (s "tag") <> semi
       , vcat $ map encField combinationFields
       , genOk genUnit
       ]
  where
    tagType = genTagTypeName combinationTag
    encBitField field =
        s "tag |=" <+> parens (s "self." <> fName <> s ".is_some() as" <+> tagType) <+> s "<<" <+> fIdx <> semi
      where
        fIdx  = s . show $ S.fieldIndex field
        fName = genStructFieldName $ S.fieldName field

    encField field = case field of
      S.DataField {..} -> genMatch (s "self." <> fName)
                          [ (genSome (s "ref a"), genTryEncode (s "a"))
                          , (s "None", s "()")
                          ]
      _ -> genComment (s "No data for field" <+> fName)
      where
        fName = genStructFieldName $ S.fieldName field

genEncodeInner S.Type {typeDesc = S.Union {..}, ..} =
  vcat [ genMatch (s "self") (map genEncodeEnumMatchArm unionFields) <> semi
       , genOk genUnit
       ]
  where
    name    = genTypeName typeName
    tagType = genTagTypeName unionTag

    genEncodeEnumMatchArm :: S.Field -> (Doc,Doc)
    genEncodeEnumMatchArm field = (pattern, exprs)
      where
        variantName = genFieldName (S.fieldName field)
        idx = s (show (S.fieldIndex field))
        (pattern, exprs) = case field of
          S.EmptyField {..} ->
            ( genRef (name <::> variantName)
            , genBlock (vcat [ s "let tag:" <+> tagType <+> equals <+> idx <> semi
                             , genTryEncode (s "tag") <> semi
                             ]
                       )
            )
          S.DataField {..}  ->
            (genRef (name <::> variantName <> parens (s "ref val"))
            , genBlock (vcat [ s "let tag:" <+> tagType <+> equals <+> idx <> semi
                             , genTryEncode (s "tag") <> semi
                             , genTryEncode (s "val") <> semi
                             ]
                       )
            )


genDecode :: S.Type -> Doc
genDecode tp =
  vcat [ s "fn decode(ctx: &mut Decoder) -> Result<Self, Error>" <+> lbrace
       , indent $ genDecodeInner tp
       , rbrace
       ]

genDecodeInner :: S.Type -> Doc
genDecodeInner S.Type {typeDesc = S.Synonym {..}, ..} =
  genOk (genTypeName typeName <> parens (genTryDecode innerType))
  where
    innerType = genTypeName synonymRef

genDecodeInner S.Type {typeDesc = S.Range {..}, ..} =
  vcat [ s "let tag =" <+> genTryDecode tagType <+> s "as" <+> primType <> semi
       , genTypeName typeName <> s "::new" <> parens (s "tag +" <+> offset)
       ]
  where
    offset = s . show $ rangeOffset
    tagType = genTagTypeName rangeTag
    primType = genPrimTypeName rangePrim

genDecodeInner S.Type {typeDesc = S.Array {..}, ..} =
  vcat [ s "let mut arr:" <+> brackets (elType <> semi <+> sz)
         <+> equals <+> genUninitialized <> semi
       , genFor (s "i in 0.." <> sz)
         (s "arr[i] =" <+> genTryDecode elType <> semi)
       , genOk (genTypeName typeName <> parens (s "arr"))
       ]
  where
    elType = genTypeName arrayRef
    sz     = s $ show arrayLength

genDecodeInner S.Type {typeDesc = S.Vector {..}, ..} =
  vcat [ s "let len =" <+> genTry (tagType <> s "::decode(ctx)") <+> s "as usize;"
       , genIf (s "len >" <+> maxLen)
         (s "return Err(Error::ElementCount);")
       , s "let mut v =" <+> name <> s "::new();"
       , genFor (s "_ in 0..len")
         (s "v.push" <> parens (genTry (elType <> s "::decode(ctx)")) <> semi)
       , genOk (s "v")
       ]
  where
    name    = genTypeName typeName
    tagType = genTagTypeName vectorTag
    maxLen  = s $ show vectorLength
    elType  = genTypeName vectorRef

genDecodeInner S.Type {typeDesc = S.Enumeration {..}, ..} =
  vcat [ s "let tag =" <+> genTryDecode tagType <> semi
       , genOk $ genMatch (s "tag") matchArms
       ]
  where
    name = genTypeName typeName
    tagType = genTagTypeName enumerationTag
    matchArms = map genDecodeEnumMatchArm enumerationValues ++ [(s "_", s "return Err(Error::InvalidTag)")]
    genDecodeEnumMatchArm S.EnumVal{..} = (s . show $ enumValIndex, name <::> genFieldName enumValName)

genDecodeInner S.Type {typeDesc = S.Record {..}, ..} =
  vcat [ s "let rec =" <+> genTypeName typeName
         <+> genBlock ( vcat (map decField recordFields)
                      ) <> semi
       , genOk(s "rec")
       ]
  where
    decField S.DataField {..} =
      genStructFieldName fieldName <> colon <+> genTryDecode (genTypeName fieldRef) <> comma
    decField S.EmptyField {} =
      error "A record shound not have an empty field"

genDecodeInner S.Type {typeDesc = S.Combination {..}, ..} =
  vcat [ s "let tag =" <+> genTryDecode tagType <> semi
       , s "let combo =" <+> genTypeName typeName
         <+> genBlock ( vcat (map decField combinationFields)
                      ) <> semi
       , genOk (s "combo")
  ]
  where
    tagType = genTagTypeName combinationTag
    decField field = case field of
      S.EmptyField {..} ->
        fName <> colon
        <+> genMatch (s "tag &" <+> parens(s "1 <<" <+> fIdx) <+> s "== 0")
                     [ (s "true",  s "None")
                     , (s "false", genSome genUnit)
                     ] <> comma

      S.DataField {..} ->
        fName <> colon
        <+> genMatch (s "tag &" <+> parens(s "1 <<" <+> fIdx) <+> s "== 0")
                     [ (s "true",  s "None")
                     , (s "false", genSome (genTry $ genTypeName fieldRef <::> s "decode(ctx)"))
                     ] <> comma
      where
        fIdx  = s . show $ S.fieldIndex field
        fName = genStructFieldName $ S.fieldName field

genDecodeInner S.Type {typeDesc = S.Union {..}, ..} =
  vcat [ s "let tag =" <+> genTryDecode tagType <> semi
       , genOk $ genMatch (s "tag") matchArms
       ]
  where
    name = genTypeName typeName
    tagType = genTagTypeName unionTag
    matchArms = map genDecodeEnumMatchArm unionFields
                ++ [(s "_", s "return Err(Error::InvalidTag)")]
    genDecodeEnumMatchArm :: S.Field -> (Doc,Doc)
    genDecodeEnumMatchArm field = case field of
      S.EmptyField {..} -> (idx , name <::> variantName)
      S.DataField {..}  -> (idx , name <::> variantName <> parens (genTryDecode variantType))
      where
        variantType = genTypeName (S.fieldRef field)
        variantName = genFieldName (S.fieldName field)
        idx = s (show (S.fieldIndex field))
