{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Cauterize.RustRef.Generate
       ( genRust
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
instance Pretty Attribute where
  pretty Debug     = s "Debug"
  pretty Default   = s "Default"
  pretty PartialEq = s "PartialEq"

genDerive :: [Attribute] -> Doc
genDerive attrs = s "#[derive" <> parens (cat (punctuate comma (map pretty attrs))) <> rbracket

s :: String -> Doc
s = string

t :: T.Text -> Doc
t = s . T.unpack

indent :: Doc -> Doc
indent = L.indent 4

intercalate :: Doc -> [Doc] -> Doc
intercalate s elems = sep (punctuate s elems)

renderDoc :: Doc -> String
renderDoc d = displayS (renderPretty 0.4 80 d) ""

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


-----------------------
-- Source generation --
-----------------------

genRust :: S.Specification -> T.Text
genRust = T.pack . genSource

genSource :: S.Specification -> String
genSource spec = renderDoc $ vcat $ punctuate empty
  [ s "#![allow(dead_code,unused_variables)]"
  , s "extern crate cauterize;"
  , s "use self::cauterize::{Encoder, Decoder, Cauterize};"
  , s "pub use self::cauterize::Error;"
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
genType tp = case S.typeDesc tp of
    S.Array{}       -> genDerive [Debug,PartialEq] <$$> genArrayArray nm tp
    S.Combination{} -> genDerive [Debug,PartialEq] <$$> genCombinationStruct nm tp
    S.Enumeration{} -> genDerive [Debug,PartialEq] <$$> genEnumerationEnum nm tp
    S.Range{}       -> range2unimplemented nm tp
    S.Record{}      -> genDerive [Debug,PartialEq] <$$> genRecordStruct nm tp
    S.Synonym{}     -> genDerive [Debug,PartialEq] <$$> genSynonymNewtype nm tp
    S.Union{}       -> genDerive [Debug,PartialEq] <$$> genUnionEnum nm tp
    S.Vector{}      -> genDerive [Debug,PartialEq] <$$> genVectorVec nm tp
    where
      nm = genTypeName $ S.typeName tp

genVec :: Doc -> Doc
genVec d = s "Vec" <> angles d

genNewType :: Doc -> Doc -> Doc
genNewType nm tp = s "pub struct"
            <+> nm
            <>  parens (s "pub" <+> tp)
            <>  semi

genEnum :: Doc -> [Doc] -> Doc
genEnum nm fields = vcat
  [s "pub enum" <+> nm <+> lbrace
  , indent (vcat fields)
  , rbrace
  ]

genStruct :: Doc -> [Doc] -> Doc
genStruct nm fields = vcat
  [s "pub struct" <+> nm <+> lbrace
  , indent (vcat fields)
  , rbrace
  ]

comment :: Doc -> Doc
comment d = s "//" <+> d

range2unimplemented :: Doc -> S.Type -> Doc
range2unimplemented nm tp = vcat
  [ comment $ s "Range type not yet implemented."
  , genEnum nm [empty]
  ]

genCombinationStruct :: Doc -> S.Type -> Doc
genCombinationStruct nm tp = genStruct nm fields
  where
    fields = genFields tp

genRecordStruct :: Doc -> S.Type -> Doc
genRecordStruct nm tp = genStruct nm fields
  where
    fields = genFields tp

genEnumerationEnum :: Doc -> S.Type -> Doc
genEnumerationEnum nm tp = genEnum nm fields
  where
    fields = genFields tp

genArrayArray :: Doc -> S.Type -> Doc
genArrayArray nm tp = genNewType nm (brackets $ elType <> semi <+> sz)
  where
    td     = S.typeDesc tp
    elType = genTypeName . S.arrayRef $  td
    sz     = s . show . S.arrayLength $ td

genVectorVec :: Doc -> S.Type -> Doc
genVectorVec nm tp = genNewType nm (genVec elType)
  where
    td     = S.typeDesc tp
    elType = genTypeName . S.vectorRef $  td

genUnionEnum :: Doc -> S.Type -> Doc
genUnionEnum nm tp = genEnum nm fields
  where
    fields = genFields tp

genSynonymNewtype :: Doc -> S.Type -> Doc
genSynonymNewtype nm tp = genNewType nm st
  where
    td = S.typeDesc tp
    sr = S.synonymRef td
    st = genTypeName sr

genStructField :: S.Field -> Doc
genStructField (S.DataField n i r) =
  s "pub" <+> nm <> colon <+> fieldType <> comma <+> comment idx
  where
    nm        = t . C.unIdentifier $  n
    fieldType = genTypeName r
    idx       = s . show $ i
genStructField (S.EmptyField _ _) = empty

genEnumField :: S.Field -> Doc
genEnumField (S.DataField n i r) =
  nm <> parens fieldType <> comma <+> comment idx
  where
    nm        = t . titleCase . C.unIdentifier $  n
    fieldType = genTypeName r
    idx       = s . show $ i
genEnumField (S.EmptyField n i) =
  nm <> comma <+> comment idx
  where
    nm        = t . titleCase . C.unIdentifier $  n
    idx       = s . show $ i

genOption :: Maybe Doc -> Doc
genOption ot = s "Option" <> angles a
  where
   a = fromMaybe (s "()") ot

genComboStructField :: S.Field -> Doc
genComboStructField (S.DataField n i r) =
  s "pub" <+> nm <> colon <+> genOption fieldType <> comma <+> comment idx
  where
    nm        = t . C.unIdentifier $ n
    fieldType = Just $ genTypeName r
    idx       = s . show $ i
genComboStructField (S.EmptyField n i) =
  s "pub" <+> nm <> colon <+> genOption Nothing <> comma <+> comment idx
  where
    nm  = t . C.unIdentifier $ n
    idx = s . show $ i

genEnumerationEnumField :: S.EnumVal -> Doc
genEnumerationEnumField (S.EnumVal n i) = nm <> comma <+> comment idx
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
               <+> braces
                   ( linebreak
                     <> indent (genEncodeInner tp)
                     <> linebreak
                   )

genEncodeInner :: S.Type -> Doc
genEncodeInner tp =
  case S.typeDesc tp of
    S.Array{..}     -> genEncodeArray nm arrayRef arrayLength
    S.Combination{} -> s "unimplemented!();"
    S.Enumeration{} -> s "unimplemented!();"
    S.Range{}       -> s "unimplemented!();"
    S.Record{}      -> s "unimplemented!();"
    S.Synonym{..}   -> genEncodeNewtype nm
    S.Union{}       -> s "unimplemented!();"
    S.Vector{}      -> s "unimplemented!();"
    where
      nm = genTypeName . S.typeName $ tp

genDecode :: S.Type -> Doc
genDecode tp = vcat
  [ s "fn decode(ctx: &mut Decoder) -> Result<Self, Error>" <+> lbrace
  , indent $ genDecodeInner tp
  , rbrace
  ]

genDecodeInner :: S.Type -> Doc
genDecodeInner tp =
  case S.typeDesc tp of
    S.Array {..}    -> genDecodeArray nm arrayRef arrayLength
    S.Combination{} -> s "unimplemented!();"
    S.Enumeration{} -> s "unimplemented!();"
    S.Range{}       -> s "unimplemented!();"
    S.Record{}      -> s "unimplemented!();"
    S.Synonym{..}   -> genDecodeNewtype nm synonymRef
    S.Union{}       -> s "unimplemented!();"
    S.Vector{}      -> s "unimplemented!();"
    where
      nm = genTypeName . S.typeName $ tp

genDecodeArray :: Doc -> C.Identifier -> C.Length -> Doc
genDecodeArray nm id len = vcat
  [ s "let mut arr:" <+> brackets (elType <> semi <+> sz)
                     <+> equals
                     <+> genUnsafe (s "mem::uninitialized()") <> semi
  , s "for i in 0.." <> sz <+> lbrace
  , indent (s "arr[i] = try!" <> parens (elType <> s "::decode(ctx)") <> semi)
  , rbrace
  , s "Ok" <> parens (nm <> parens (s "arr"))
  ]
  where
    elType = genTypeName id
    sz     = s . show $ len

genEncodeArray :: Doc -> C.Identifier -> C.Length -> Doc
genEncodeArray nm id len = s "let ref elems = self.0;"
                      <$$> s "for elem in elems.iter()"
                       <+> braces
                           ( linebreak
                             <> indent (s "try!(elem.encode(ctx));")
                             <> linebreak
                           )
                      <$$> s "Ok(())"
  where
    elType = genTypeName id
    sz     = s . show $ len

genEncodeNewtype :: Doc -> Doc
genEncodeNewtype nm = s "let &" <> nm <> parens (s "ref inner") <+> s "= self;"
                 <$$> genTry (s "inner.encode(ctx)") <> semi
                 <$$> genOk (parens empty)

genDecodeNewtype :: Doc -> C.Identifier -> Doc
genDecodeNewtype nm id = genOk (nm <> parens (genTry (innerType <> s "::decode(ctx)")))
  where
    innerType = genTypeName id
