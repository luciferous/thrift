{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Generator where

import Prelude hiding (const)

import Data.List (intercalate)
import Language.Haskell.Exts.Parser (parseModule, ParseResult(..))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Text.Printf (printf)

import Types

toHask :: BaseType -> String
toHask "binary" = "String"
toHask "bool"   = "Bool"
toHask "byte"   = "Char"
toHask "string" = "String"
toHask "i64"    = "Int64"
toHask "i32"    = "Int32"
toHask x = x

class Generator a where
    gen :: a -> String

instance Generator Definition where
    gen (Typedef dtype ident) =
        printf "type %s = %s\n" ident (toHask (gen dtype))
    gen (Const ft ident val) = unlines
        [ ident ++ " :: " ++ gen ft
        , ident ++ " = " ++ gen val
        ]
    gen (Service ident _ funcs) = ""
    gen (Struct ident []) = "data " ++ ident
    gen (Struct ident fields) =
        printf "data %s = %s {%s}\n" ident ident $ gen (", ", fields)
    gen _ = ""

instance Generator g => Generator [g] where
    gen gs = foldr (++) "" (map gen gs)

instance Generator g => Generator (String, [g]) where
    gen (d, gs) = intercalate d (map gen gs)

instance Generator Field where
    gen Field{..} = printf "_%s :: %s" _fieldName (gen _fieldType)

instance Generator DefinitionType where
    gen (Left baseType) = baseType
    gen (Right contType) = gen contType

instance Generator FieldType where
    gen (Identifier t) = t
    gen (BaseType t) = toHask t
    gen (ContainerType t) = gen t

instance Generator ContainerType where
    gen (MapType _ (k, v)) = printf "[(%s, %s)]" (gen k) (gen v)
    gen (SetType _ ft) = printf "[%s]" (gen ft)
    gen (ListType ft _) = printf "[%s]" (gen ft)

instance Generator ConstValue where
    gen (ConstNumber (Left x)) = show x
    gen (ConstNumber (Right x)) = show x
    gen (ConstLiteral s) = show s
    gen (ConstIdentifier i) = i

exts :: String
exts = unlines $ map wrap [ "EmptyDataDecls" ]
  where
    wrap x = "{-# LANGUAGE " ++ x ++ " #-}"

generate :: Document -> String
generate (Document _ defs) =
    let p1 = exts ++ "\n" ++ (gen ("\n", defs))
        p2 = parseModule p1
     in case p2 of
            ParseOk p -> prettyPrint p
            ParseFailed loc err -> p1 ++ "\n\n" ++ err ++ "\n\n" ++ (show loc)
