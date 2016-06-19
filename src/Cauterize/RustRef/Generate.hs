{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Cauterize.RustRef.Generate
       ( genRust
       , genManifest
       ) where

import qualified Cauterize.CommonTypes   as C
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

genDerive :: [Attribute] -> Doc
genDerive attrs = s "#[derive" <> parens (intercalate comma (map pretty attrs)) <> rbracket

s :: String -> Doc
s = string

t :: T.Text -> Doc
t = s . T.unpack

indent :: Doc -> Doc
indent = L.indent 4

intercalate :: Doc -> [Doc] -> Doc
intercalate seperator elems = sep (punctuate seperator elems)

renderDoc :: Doc -> String
renderDoc d = displayS (renderPretty 1.0 80 d) ""

genTypeName :: C.Identifier -> Doc
genTypeName n = s . cautTypeToRustType $ n

genFieldName :: C.Identifier -> Doc
genFieldName = s . cautFieldToRustField

genUnsafe :: Doc -> Doc
genUnsafe d = s "unsafe" <+> braces (space <> d <> space)

genTry :: Doc -> Doc
genTry d = s "try!" <> parens d

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
    matchArm (l,r) = fill 2 l <+> s "=>" <+> r
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

genUnitialized :: Doc
genUnitialized = genUnsafe $ s "mem::uninitialized()"

genPrimTypeName :: C.Prim -> Doc
genPrimTypeName = t . cautPrimToRustPrim


---------------------------
  -- Cargo.toml generation --
---------------------------

genManifest :: S.Specification -> T.Text
genManifest spec= T.pack . renderDoc $ vcat
  [ s "[package]"
  , s "name =" <+> dquotes specName
  , s "version = \"0.1.0\"" -- TODO: the following does not work due to cargo's semver requirements: dquotes specVersion
  , s "authors = [\"author\"]"
  , empty
  , s "[lib]"
  , s "name =" <+> dquotes specName
  , s "path =" <+> dquotes (s "src/" <> specName <> s ".rs")
  , empty
  , s "[dependencies]"
  , s "byteorder = \"0.5\""
  , empty
  , s "[dev-dependencies]"
  , s "quickcheck = \"0.2\""
  , s "quickcheck_macros = \"0.2\""
  ]
  where
    specName    = t . S.specName    $ spec
    specVersion = t . S.specVersion $ spec

-----------------------
-- Source generation --
-----------------------

genRust :: S.Specification -> T.Text
genRust = T.pack . genSource

genSource :: S.Specification -> String
genSource spec = renderDoc $ vcat $ punctuate empty
  [ s "#![cfg_attr(test, feature(plugin))]"
  , s "#![cfg_attr(test, plugin(quickcheck_macros))]"
  , s "#![allow(dead_code,unused_variables)]"
  , s "#![feature(associated_consts)]"
  , empty
  , s "#[cfg(test)]"
  , s "#[macro_use]"
  , s "extern crate quickcheck;"
  , s "mod cauterize;"
  , s "use self::cauterize::{Error, Encoder, Decoder, Cauterize, Range};"
  , s "use std::mem;"
  , empty
  , s "pub static SPEC_NAME: &'static str =" <+> dquotes specName <> semi
  , empty
  , vcat [  genType tp <> linebreak
         <> empty <> linebreak
         <> genImpl tp <> linebreak
         | tp <- S.specTypes spec
         ]
  , empty
  ]
  where
    specName = t . S.specName $ spec


---------------------
-- Type generation --
---------------------

genType :: S.Type -> Doc
genType tp@S.Type{..} = case typeDesc of
    S.Array{}       -> genArrayArray nm tp
    S.Combination{} -> genCombinationStruct nm tp
    S.Enumeration{} -> genEnumerationEnum nm tp
    S.Record{}      -> genRecordStruct nm tp
    S.Synonym{}     -> genSynonymNewtype nm tp
    S.Union{}       -> genUnionEnum nm tp
    S.Vector{}      -> genVectorVec nm tp
    S.Range{}       -> vcat [ genRange nm typeDesc
                            , empty
                            , genRangeImpl nm typeDesc
                            ]
    where
      nm = genTypeName typeName

genVec :: Doc -> Doc
genVec d = s "Vec" <> angles d

genNewType :: Doc -> Bool ->  Doc-> Doc
genNewType nm pub tp  =
  s "pub struct" <+> nm <>  parens (visibility <> tp) <>  semi
  where
    visibility | pub == True = s "pub "
               | otherwise   = empty

genEnum :: Doc -> [Doc] -> Doc
genEnum nm fields = vcat
  [ s "pub enum" <+> nm <+> lbrace
  , indent (vcat fields)
  , rbrace
  ]

genStruct :: Doc -> [Doc] -> Doc
genStruct nm fields = vcat
  [ s "pub struct" <+> nm <+> lbrace
  , indent (vcat fields)
  , rbrace
  ]

genRange :: Doc -> S.TypeDesc -> Doc
genRange nm S.Range{..} = genNewType nm False primType
  where
    primType = genPrimTypeName rangePrim


genRangeImpl :: Doc -> S.TypeDesc -> Doc
genRangeImpl nm S.Range {..} =
  s "impl Range for" <+> nm <+> genBlock
  ( vcat
    [ s "type P =" <+> primType <> semi
    , s "type T =" <+> tagType <> semi
    , s "const OFFSET:" <+> primType <+> equals <+> offset <> semi
    , s "const LENGTH:" <+> primType <+> equals <+> len <> semi
    , s "fn new(val: Self::P) -> Result<Self,Error>" <+> genBlock
      (
        vcat
        [ s "if (Self::OFFSET <= val) && (val <= Self::OFFSET + Self::LENGTH)" <+> genBlock
          ( s "return" <+> genOk (nm <> parens (s "val")) <> semi
          )
        , s "Err(Error::OutOfRange)"
        ]
      )
    , s "fn set" <> parens (s "&mut self, val:" <+> primType) <+> s "-> Option" <> angles primType <+> genBlock
      (
        vcat
        [ s "if (Self::OFFSET <= val) && (val <= Self::OFFSET + Self::LENGTH)" <+> genBlock
          ( vcat
            [ s "self.0 = val;"
            , s "return None;"
            ]
          )
        , s "Some(val)"
        ]
      )
    , s "fn get(&self) ->" <+> primType <+> genBlock ( s "self.0")
    ]
  )
  where
    offset = s . show $ rangeOffset
    len = s . show $ rangeLength
    tagType = genTagTypeName rangeTag
    primType = genPrimTypeName rangePrim

genCombinationStruct :: Doc -> S.Type -> Doc
genCombinationStruct nm tp = genStruct nm fields
  where
    fields = genFields tp

genRecordStruct :: Doc -> S.Type -> Doc
genRecordStruct nm tp = genStruct nm fields
  where
    fields = genFields tp

genEnumerationEnum :: Doc -> S.Type -> Doc
genEnumerationEnum nm tp = vcat
  [ genRepr tagType
  , genEnum nm fields
  ]
  where
    fields  = genFields tp
    td      = S.typeDesc tp
    tagType = genTagTypeName . S.enumerationTag $  td

genArrayArray :: Doc -> S.Type -> Doc
genArrayArray nm tp = genNewType nm True (brackets $ elType <> semi <+> sz)
  where
    td     = S.typeDesc tp
    elType = genTypeName . S.arrayRef $  td
    sz     = s . show . S.arrayLength $ td

genVectorVec :: Doc -> S.Type -> Doc
genVectorVec nm tp = genNewType nm True (genVec elType)
  where
    td     = S.typeDesc tp
    elType = genTypeName . S.vectorRef $  td

genUnionEnum :: Doc -> S.Type -> Doc
genUnionEnum nm tp = genEnum nm fields
  where
    fields = genFields tp

genSynonymNewtype :: Doc -> S.Type -> Doc
genSynonymNewtype nm tp = genNewType nm True st
  where
    td = S.typeDesc tp
    sr = S.synonymRef td
    st = genTypeName sr

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
genEnumerationEnumField (S.EnumVal n i) = nm <> comma <+> genComment idx
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

genImpl :: S.Type -> Doc
genImpl tp = vcat
  [ s "impl Cauterize for" <+> nm <+> lbrace
  , indent $ genEncode tp
  , empty
  , indent $ genDecode tp
  , rbrace
  ]
  where
    nm = genTypeName . S.typeName $ tp

genEncode :: S.Type -> Doc
genEncode tp = s "fn encode(&self, ctx: &mut Encoder) -> Result<(), Error>"
           <+> genBlock (genEncodeInner tp)

genEncodeInner :: S.Type -> Doc
genEncodeInner tp@S.Type {..} =
  case typeDesc of
    S.Array{}       -> genEncodeArray
    S.Combination{} -> genEncodeCombinationStruct nm typeDesc
    S.Enumeration{} -> genEncodeEnumerationEnum tp
    S.Range{}       -> genEncodeRange nm typeDesc
    S.Record{}      -> genEncodeStruct typeDesc
    S.Synonym{..}   -> genEncodeNewtype nm
    S.Union{}       -> genEncodeEnum nm tp
    S.Vector{}      -> genEncodeVec nm typeDesc
    where
      nm = genTypeName typeName

genDecode :: S.Type -> Doc
genDecode tp =
  vcat [ s "fn decode(ctx: &mut Decoder) -> Result<Self, Error>" <+> lbrace
       , indent $ genDecodeInner tp
       , rbrace
       ]

genDecodeInner :: S.Type -> Doc
genDecodeInner tp@S.Type {..} =
  case typeDesc of
    S.Array {..}    -> genDecodeArray nm arrayRef arrayLength
    S.Combination{} -> genDecodeCombinationStruct nm typeDesc
    S.Enumeration{} -> genDecodeEnumerationEnum tp
    S.Range{}       -> genDecodeRange nm typeDesc
    S.Record{}      -> genDecodeStruct nm typeDesc
    S.Synonym{..}   -> genDecodeNewtype nm synonymRef
    S.Union{}       -> genDecodeEnum nm tp
    S.Vector{}      -> genDecodeVec nm typeDesc
    where
      nm = genTypeName typeName

genDecodeArray :: Doc -> C.Identifier -> C.Length -> Doc
genDecodeArray nm ident len = vcat
  [ s "let mut arr:" <+> brackets (elType <> semi <+> sz)
    <+> equals <+> genUnitialized <> semi
  , genFor (s "i in 0.." <> sz)
    (s "arr[i] =" <+> genTryDecode elType <> semi)
  , genOk (nm <> parens (s "arr"))
  ]
  where
    elType = genTypeName ident
    sz     = s . show $ len

genEncodeArray :: Doc
genEncodeArray = vcat
  [ s "let ref elems = self.0;"
  , genFor (s "elem in elems.iter()")
           (s "try!(elem.encode(ctx));")
  , genOk genUnit
  ]

genEncodeNewtype :: Doc -> Doc
genEncodeNewtype nm = vcat
  [ s "let &" <> nm <> parens (s "ref inner") <+> s "= self;"
  , genTryEncode (s "inner") <> semi
  , genOk (parens empty)
  ]

genDecodeNewtype :: Doc -> C.Identifier -> Doc
genDecodeNewtype nm ident = genOk (nm <> parens (genTryDecode innerType))
  where
    innerType = genTypeName ident

genEncodeEnumerationEnum :: S.Type -> Doc
genEncodeEnumerationEnum tp = vcat
  [ s "let tag: &" <> tagType <+> s "= unsafe { mem::transmute(self) };"
  , s "try!(tag.encode(ctx));"
  , genOk genUnit
  ]
  where
    td      = S.typeDesc tp
    tagType = genTagTypeName . S.enumerationTag $ td

genDecodeEnumerationEnum :: S.Type -> Doc
genDecodeEnumerationEnum tp = vcat
  [ s "let tag = " <> genTryDecode tagType <> semi
  , genIf (s "tag > " <> maxTag)
          (s "return Err(Error::InvalidTag);")
  , s "Ok(unsafe { mem::transmute(tag) })"
  ]
  where
    td      = S.typeDesc tp
    tagType = genTagTypeName . S.enumerationTag $ td
    maxTag  = s . show $ ((length . S.enumerationValues $ td) - 1)

genEncodeEnum :: Doc -> S.Type -> Doc
genEncodeEnum nm tp = vcat
  [ genMatch (s "self") (map genEncodeEnumMatchArm (S.unionFields td)) <> semi
  , genOk genUnit
  ]
  where
    td = S.typeDesc tp
    tagType = genTagTypeName . S.unionTag $ td
    genEncodeEnumMatchArm :: S.Field -> (Doc,Doc)
    genEncodeEnumMatchArm field = (pattern, exprs)
      where
        variantName = genFieldName (S.fieldName field)
        idx = s (show (S.fieldIndex field))
        (pattern, exprs) = case field of
          S.EmptyField {..} -> ( genRef (nm <::> variantName)
                               , genBlock (vcat [ s "let tag:" <+> tagType <+> equals <+> idx <> semi
                                                , genTryEncode (s "tag") <> semi
                                                ]
                                          )
                               )
          S.DataField {..}  -> (genRef (nm <::> variantName <> parens (s "ref val"))
                               , genBlock (vcat [ s "let tag:" <+> tagType <+> equals <+> idx <> semi
                                                , genTryEncode (s "tag") <> semi
                                                , genTryEncode (s "val") <> semi
                                                ]
                                          )
                               )

genDecodeEnum :: Doc -> S.Type -> Doc
genDecodeEnum nm tp = vcat
  [ s "let tag =" <+> genTryDecode tagType <> semi
  , genMatch (s "tag") matchArms
  ]
  where
    td = S.typeDesc tp
    tagType = genTagTypeName . S.unionTag $ td
    matchArms = map genDecodeEnumMatchArm (S.unionFields td)
                ++ [(s "_", s "Err(Error::InvalidTag),")]
    genDecodeEnumMatchArm :: S.Field -> (Doc,Doc)
    genDecodeEnumMatchArm field = case field of
      S.EmptyField {..} -> (idx , genOk (nm <::> variantName) <> comma)
      S.DataField {..}  -> (idx , genOk (nm <::> variantName <> parens
                                          (genTryDecode variantType)
                                        ) <> comma
                           )
      where
        variantType = genTypeName (S.fieldRef field)
        variantName = genFieldName (S.fieldName field)
        idx = s (show (S.fieldIndex field))

genEncodeStruct :: S.TypeDesc -> Doc
genEncodeStruct S.Record {..} = vcat (map genEncodeStructField recordFields)
                                <$$> genOk genUnit
  where
    genEncodeStructField field = genTryEncode (s "self." <> fieldName) <> semi
      where
        fieldName = genStructFieldName (S.fieldName field)

genDecodeStruct :: Doc -> S.TypeDesc -> Doc
genDecodeStruct nm S.Record {..} = vcat
  [ s "let rec =" <+> nm <+> genBlock (vcat (map decField recordFields)) <> semi
  , genOk(s "rec")
  ]
  where
    decField S.DataField {..} = genStructFieldName fieldName <> colon
                            <+> genTryDecode (genTypeName fieldRef) <> comma

genEncodeVec :: Doc -> S.TypeDesc -> Doc
genEncodeVec nm S.Vector {..} = vcat
  [ s "let len = self.0.len();"
  , genIf (s "len >=" <+> maxLen)
          (s "return Err(Error::ElementCount);")
  , genTryEncode (parens (s "len as" <+> tagType)) <> semi
  , genFor (s "elem in self.0.iter()")
           (genTryEncode (s "elem"))
  , genOk genUnit
  ]
  where
    tagType = genTagTypeName vectorTag
    maxLen = s . show $ vectorLength

genDecodeVec :: Doc -> S.TypeDesc -> Doc
genDecodeVec nm S.Vector {..} = vcat
  [ s "let len =" <+> genTry (tagType <> s "::decode(ctx)") <+> s "as usize;"
  , genIf (s "len >=" <+> maxLen)
          (s "return Err(Error::ElementCount);")
  , s "let mut v: Vec" <> angles elType <+> s "= Vec::with_capacity(len);"
  , genFor (s "_ in 0..len")
           (s "v.push" <> parens (genTry (elType <> s "::decode(ctx)")) <> semi)
  , genOk (nm <> parens (s "v"))
  ]
  where
    tagType = genTagTypeName vectorTag
    maxLen = s . show $ vectorLength
    elType = genTypeName vectorRef

genEncodeCombinationStruct :: Doc -> S.TypeDesc -> Doc
genEncodeCombinationStruct nm S.Combination {..} = vcat
  [ s "let mut tag:" <+> tagType <+> s "= 0;"
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
                          [ (genSome (s "ref a"), genTryEncode (s "a") <> comma)
                          , (s "None", s "()"  <> comma)
                          ]
      _ -> genComment (s "No data for field" <+> fName)
      where
        fName = genStructFieldName $ S.fieldName field

genDecodeCombinationStruct :: Doc -> S.TypeDesc -> Doc
genDecodeCombinationStruct nm S.Combination {..} = vcat
  [ s "let tag =" <+> genTryDecode tagType <> semi
  , s "let combo =" <+> nm <+> genBlock (vcat (map decField combinationFields))
    <> semi
  , genOk (s "combo")
  ]
  where
    tagType = genTagTypeName combinationTag
    decField field = case field of
      S.EmptyField {..} ->
        fName <> colon
        <+> genMatch (s "tag &" <+> parens(s "1 <<" <+> fIdx) <+> s "== 0")
                     [ (s "true",  s "None"  <> comma)
                     , (s "false", genSome genUnit <> comma)
                     ] <> comma

      S.DataField {..} ->
        fName <> colon
        <+> genMatch (s "tag &" <+> parens(s "1 <<" <+> fIdx) <+> s "== 0")
                     [ (s "true",  s "None"  <> comma)
                     , (s "false", genSome (genTry $ genTypeName fieldRef <::> s "decode(ctx)") <> comma)
                     ] <> comma
      where
        fIdx  = s . show $ S.fieldIndex field
        fName = genStructFieldName $ S.fieldName field

genEncodeRange :: Doc -> S.TypeDesc -> Doc
genEncodeRange nm S.Range {..} = vcat
  [ s "let tag =" <+> parens (s "self.0 +" <+> offset) <+> s "as" <+> tagType <> semi
  , genOk . genTryEncode $ s "tag"
  ]
  where
    offset = s . show $ rangeOffset
    tagType = genTagTypeName rangeTag
    primType = genPrimTypeName rangePrim

genDecodeRange :: Doc -> S.TypeDesc -> Doc
genDecodeRange nm S.Range {..} = vcat
  [ s "let tag =" <+> genTryDecode tagType <+> s "as" <+> primType <> semi
  , nm <> s "::new" <> parens (s "tag +" <+> offset)
  ]
  where
    offset = s . show $ rangeOffset
    tagType = genTagTypeName rangeTag
    primType = genPrimTypeName rangePrim
