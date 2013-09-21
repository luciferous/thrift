{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (takeWhile)
import Control.Applicative hiding (Const)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Char8
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B hiding (map)
import System.Environment (getArgs)
import GHC.Conc (getNumProcessors, setNumCapabilities)

type Identifier = ByteString

type Scope = ByteString

data Document   = Document [Header] [Definition]
                  deriving Show

data Header     = Include ByteString
                | CppInclude ByteString
                | Namespace Scope Identifier
                  deriving Show

data Definition = Const FieldType Identifier ConstValue
                | Typedef DefinitionType Identifier
                | Enum [(Identifier, Maybe Int)]
                | Senum Identifier [ByteString]
                | Struct Identifier [Field]
                | Exception Identifier [Field]
                | Service Identifier (Maybe Parent) [Function]
                  deriving Show

type Parent     = Identifier

data Field      = Field { _fieldId    :: Maybe Int
                        , _fieldReq   :: Maybe Bool
                        , _fieldType  :: FieldType
                        , _fieldName  :: Identifier
                        , _fieldValue :: Maybe ConstValue
                        } deriving Show

data Function   = Function { _fnOneway :: Maybe Bool
                           , _fnType   :: Maybe FieldType
                           , _fnName   :: Identifier
                           , _fnThrows :: [Field]
                           } deriving Show

type FieldType  = ByteString
type ConstValue = ByteString
type DefinitionType = ByteString

document :: Parser Document
document = Document <$> (many1 header <* endOfLine)
                    <*> (many1 definition <* endOfLine)

header :: Parser Header
header = Include    <$> ("include" .*> skipSpace *> literal)
     <|> CppInclude <$> ("cpp_include" .*> skipSpace *> literal)
     <|> Namespace  <$> ("namespace" .*> skipSpace *> scope)
                    <*> (skipSpace *> identifier)

literal :: Parser ByteString
literal = char8 '"' *> takeTill (=='"')
      <|> char8 '\'' *> takeTill (=='\'')

identifier :: Parser Identifier
identifier = B.cons <$> satisfy start <*> takeWhile rest
  where
    start = inClass "A-Za-z_"
    rest  = inClass "0-9A-Za-z_"

scope :: Parser Scope
scope = string "hs"
    <|> string "java"

definition :: Parser Definition
definition = constParser
         <|> typedef
         <|> enum
         <|> senum
         <|> struct
         <|> exception
         <|> service
         <?> "unknown definition"
  where
    constParser = do 
        ft    <- "const" .*> skipSpace *> fieldType
        ident <- skipSpace *> identifier
        val   <- skipSpace *> char '=' *> skipSpace *> literal
        return $ Const ft ident val
    typedef = return $ Senum "foo" []
    enum = return $ Senum "foo" []
    senum = return $ Senum "foo" []
    struct = return $ Senum "foo" []
    exception = return $ Senum "foo" []
    service = return $ Senum "foo" []

fieldType :: Parser FieldType
fieldType = identifier

compile :: [FilePath] -> IO ()
compile files = do
    contents <- mapM B.readFile files
    --let results = map (parse document) contents
    let results = map (parse header) contents
    putStrLn (show results)
    return ()

main :: IO ()
main = do setNumCapabilities =<< getNumProcessors
          compile =<< getArgs
