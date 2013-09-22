module Syntax where

import Prelude hiding (const, takeWhile)

import Control.Applicative ((<$>),(<*),(<*>),(*>))
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

import Types

lexer :: P.TokenParser st
lexer = P.makeTokenParser $ emptyDef
    { P.commentStart   = "/*"
    , P.commentEnd     = "*/"
    , P.commentLine    = "//"
    , P.identLetter    = alphaNum <|> oneOf "_."
    }

angles = P.angles lexer
braces = P.braces lexer
colon = P.colon lexer
comma = P.comma lexer
commaSep = P.commaSep lexer
identifier = P.identifier lexer
natural = P.natural lexer
naturalOrFloat = P.naturalOrFloat lexer
parens = P.parens lexer
semi = P.semi lexer
stringLiteral = P.stringLiteral lexer
symbol = P.symbol lexer
whiteSpace = P.whiteSpace lexer

listSeparator :: Parser String
listSeparator = comma <|> semi

lang :: Parser String
lang = identifier <|> symbol "*"

fieldType :: Parser FieldType
fieldType = fmap BaseType baseType
        <|> fmap ContainerType containerType
        <|> fmap Identifier identifier

constValue :: Parser ConstValue
constValue = fmap ConstNumber naturalOrFloat
         <|> fmap ConstLiteral stringLiteral
         <|> fmap ConstIdentifier identifier

field :: Parser Field
field = do
    fid   <- optionMaybe natural <* colon
    req   <- optionMaybe $ symbol "required" *> return True
                       <|> symbol "optional" *> return False
    fType <- fieldType
    ident <- identifier
    val   <- optionMaybe (symbol "=" *> constValue) <* optional listSeparator
    return $ Field fid req fType ident val

baseType :: Parser BaseType
baseType = try (symbol "bool")
       <|> try (symbol "byte")
       <|> symbol "binary"
       <|> try (symbol "i16")
       <|> try (symbol "i32")
       <|> try (symbol "i64")
       <|> symbol "double"
       <|> try (symbol "slist")
       <|> try (symbol "string")
       <?> "basetype"

cppType :: Parser CppType
cppType = symbol "cpp_type" *> stringLiteral

containerType :: Parser ContainerType
containerType = mapType <|> setType <|> listType
  where
    mapType = symbol "map" *> do
        cpp     <- optionMaybe cppType
        mapping <- angles $ do
            keyType <- fieldType <* comma
            valType <- fieldType
            return $ (keyType, valType)
        return $ MapType cpp mapping
    setType = symbol "set" *> do
        cpp <- optionMaybe cppType
        t   <- angles fieldType
        return $ SetType cpp t
    listType = symbol "list" *> do
        t   <- angles fieldType
        cpp <- optionMaybe cppType
        return $ ListType t cpp

definitionType :: Parser DefinitionType
definitionType = fmap Left baseType <|> fmap Right containerType

function :: Parser Function
function = do
    ow <- optionMaybe (symbol "oneway" *> return True)
    ft <- symbol "void" *> return Nothing <|> fmap Just fieldType
    ident <- identifier
    fields <- parens (many field) <* optional comma
    throws <- option [] (symbol "throws" *> parens (many field))
    optional comma
    return $ Function ow ft ident fields throws

definition :: Parser Definition
definition = const
         <|> try enum
         <|> exception
--         <|> senum
         <|> try service
         <|> struct
         <|> typedef
         <?> "definitions"
  where
    const = symbol "const" *> do 
        ft    <- fieldType
        ident <- identifier <* symbol "="
        val   <- constValue
        optional semi
        return $ Const ft ident val
    typedef = symbol "typedef" *> do
        dt    <- definitionType 
        ident <- identifier
        return $ Typedef dt ident
    enum = symbol "enum" *> do
        ident <- identifier
        items <- braces $ many $ do
            key <- identifier
            val <- optionMaybe $ symbol "=" *> natural
            optional comma
            return $ (key, val)
        return $ Enum ident items
--    senum = return $ Senum "foo" []
    struct = symbol "struct" *> do
        ident <- identifier
        fields <- braces (many field)
        return $ Struct ident fields
    exception = symbol "exception" *> do
        ident <- identifier
        fields <- braces (many field)
        return $ Exception ident fields
    service = symbol "service" *> do
        ident <- identifier
        ext   <- optionMaybe (symbol "extends" *> identifier)
        functions <- braces (many function)
        return $ Service ident ext functions

header :: Parser Header
header = Include    <$> (symbol "include" *> stringLiteral)
     <|> CppInclude <$> (symbol "cpp_include" *> stringLiteral)
     <|> Namespace  <$> (symbol "namespace" *> lang) <*> identifier

document :: Parser Document
document = Document <$> many (try header) <*> many definition <* eof

thrift :: Parser Document
thrift = whiteSpace *> document
