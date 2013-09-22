{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>),(<*),(<*>),(*>))
import GHC.Conc (getNumProcessors, setNumCapabilities)
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (parseFromFile, Parser)
import qualified Text.Parsec.Token as P
import System.IO.Unsafe (unsafePerformIO)

import Types

import Prelude hiding (takeWhile)

lexer :: P.TokenParser st
lexer = P.makeTokenParser emptyDef

symbol = P.symbol lexer
stringLiteral = P.stringLiteral lexer
identifier = P.identifier lexer
braces = P.braces lexer
commaSep = P.commaSep lexer
colon = P.colon lexer
natural = P.natural lexer
naturalOrFloat = P.naturalOrFloat lexer

lang :: Parser String
lang = symbol "hs" <|> symbol "cpp" <|> symbol "java"

fieldType :: Parser FieldType
fieldType = identifier

constValue :: Parser ConstValue
constValue = fmap ConstNumber naturalOrFloat
         <|> fmap ConstLiteral stringLiteral

field :: Parser Field
field = do
    fid   <- optionMaybe natural <* colon
    req   <- optionMaybe $ symbol "required" *> return True
                       <|> symbol "optional" *> return False
    fType <- fieldType
    ident <- identifier
    val   <- optionMaybe constValue
    return $ Field fid req fType ident val

definition :: Parser Definition
definition = constParser
--         <|> typedef
--         <|> enum
--         <|> senum
         <|> struct
--         <|> exception
--         <|> service
--         <?> "unknown definition"
  where
    constParser = symbol "const" *> do 
        ft    <- fieldType
        ident <- identifier <* symbol "="
        val   <- constValue
        return $ Const ft ident val
--    typedef = return $ Senum "foo" []
--    enum = return $ Senum "foo" []
--    senum = return $ Senum "foo" []
    struct = symbol "struct" *> do
        ident <- identifier
        fields <- braces (commaSep field)
        return $ Struct ident fields
--    exception = return $ Senum "foo" []
--    service = return $ Senum "foo" []

header :: Parser Header
header = Include    <$> (symbol "include" *> stringLiteral)
     <|> CppInclude <$> (symbol "cpp_include" *> stringLiteral)
     <|> Namespace  <$> (symbol "namespace" *> lang) <*> identifier

document :: Parser Document
document = Document <$> many (try header) <*> many definition

compile :: [FilePath] -> IO ()
compile files = do
    results <- mapM (parseFromFile document) files
    putStrLn (show results)
    return ()

main :: IO ()
main = do setNumCapabilities =<< getNumProcessors
          compile =<< getArgs
