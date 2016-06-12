{-# LANGUAGE OverloadedStrings #-}

module Cauterize.RustRef.Generate
       (genRust
       ) where

import qualified Cauterize.CommonTypes   as C
import           Cauterize.RustRef.Util
import qualified Cauterize.Specification as S
import           Data.Maybe
import qualified Data.Text               as T
import           Text.PrettyPrint.Leijen


dd :: Doc
dd = s "#[derive(Debug)]"
  <> linebreak
  <> s "#[derive(PartialEq)]"

s :: String -> Doc
s = string

t :: T.Text -> Doc
t = s . T.unpack

genRust :: S.Specification -> T.Text
genRust = T.pack . genSource

genSource :: S.Specification -> String
genSource spec = renderDoc $ vcat $ punctuate empty
  [ s "#![allow(dead_code)]"
  , s "pub static SPEC_NAME:  &'static str = " <+> dquotes specName <> semi
  , empty
  , vcat [ genType tp <> linebreak
         | tp <-  S.specTypes spec
         ]
  , empty
  ]
  where
    specName = t . S.specName $ spec


renderDoc :: Doc -> String
renderDoc d = displayS (renderPretty 0.4 80 d) ""

genType :: S.Type -> Doc
genType tp = case S.typeDesc tp of
    S.Array{}       -> dd <> linebreak <> genArrayArray tp
    S.Combination{} -> dd <> linebreak <> genCombinationStruct tp
    S.Enumeration{} -> dd <> linebreak <> genEnumerationEnum tp
    S.Range{}       -> range2unimplemented tp
    S.Record{}      -> dd <> linebreak <> genRecordStruct tp
    S.Synonym{}     -> dd <> linebreak <> genSynonymNewtype tp
    S.Union{}       -> dd <> linebreak <> genUnionEnum tp
    S.Vector{}      -> dd <> linebreak <> genVectorVec tp

genVec :: Doc -> Doc
genVec d = s "Vec" <> angles d


genNewType :: Doc -> Doc -> Doc
genNewType nm tp = s "pub struct"
            <+> nm
            <>  parens tp
            <>  semi


genEnum :: Doc -> [Doc] -> Doc
genEnum nm fields = vcat
  [s "pub enum" <+> nm <+> lbrace
  , indent 4 (vcat fields)
  , rbrace
  ]


genStruct :: Doc -> [Doc] -> Doc
genStruct nm fields = vcat
  [s "pub struct" <+> nm <+> lbrace
  , indent 4 (vcat fields)
  , rbrace
  ]


comment :: Doc -> Doc
comment d = s "//" <+> d


range2unimplemented :: S.Type -> Doc
range2unimplemented tp = vcat
  [ comment $ s "Range type not yet implemented."
  , comment $ s "Not declaring range" <+> nm
  ]
  where
    nm = s. cautNameToRustName . S.typeName $ tp


genCombinationStruct :: S.Type -> Doc
genCombinationStruct tp = genStruct nm fields
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    fields = genFields tp

genRecordStruct :: S.Type -> Doc
genRecordStruct tp = genStruct nm fields
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    fields = genFields tp


genEnumerationEnum :: S.Type -> Doc
genEnumerationEnum tp = genEnum nm fields
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    fields = genFields tp


genArrayArray :: S.Type -> Doc
genArrayArray tp = s "pub struct"
             <+> nm
              <> (parens . brackets $ elType <> semi <+> sz)
              <> semi
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    td     = S.typeDesc tp
    elType = s . cautNameToRustName . S.arrayRef $  td
    sz     = s . show . S.arrayLength $ td



genVectorVec :: S.Type -> Doc
genVectorVec tp = genNewType nm (genVec elType)
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    td     = S.typeDesc tp
    elType = s . cautNameToRustName . S.vectorRef $  td



genUnionEnum :: S.Type -> Doc
genUnionEnum tp = genEnum nm fields
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    fields = genFields tp


genSynonymNewtype :: S.Type -> Doc
genSynonymNewtype tp = genNewType nm st
  where
    nm = s . cautNameToRustName . S.typeName $ tp
    td = S.typeDesc tp
    sr = S.synonymRef td
    st = s . cautNameToRustName $ sr


genStructField :: S.Field -> Doc
genStructField (S.DataField n i r) =
  s "pub" <+> nm <> colon <+> fieldType <> comma <+> comment idx
  where
    nm        = t . C.unIdentifier $  n
    fieldType = s . cautNameToRustName $ r
    idx       = s . show $ i
genStructField (S.EmptyField _ _) = empty


genEnumField :: S.Field -> Doc
genEnumField (S.DataField n i r) =
  nm <> parens fieldType <> comma <+> comment idx
  where
    nm        = t . titleCase . C.unIdentifier $  n
    fieldType = s . cautNameToRustName $ r
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
    fieldType = Just . s . cautNameToRustName $ r
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
