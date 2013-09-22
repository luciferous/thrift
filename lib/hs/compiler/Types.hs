module Types where

type Identifier = String

type Scope = String

data Document   = Document [Header] [Definition]
                  deriving Show

data Header     = Include String
                | CppInclude String
                | Namespace Scope Identifier
                  deriving Show

data Definition = Const FieldType Identifier ConstValue
                | Typedef DefinitionType Identifier
                | Enum [(Identifier, Maybe Integer)]
                | Senum Identifier [String]
                | Struct Identifier [Field]
                | Exception Identifier [Field]
                | Service Identifier (Maybe Parent) [Function]
                  deriving Show

type Parent     = Identifier

data Field      = Field { _fieldId    :: Maybe Integer
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

type FieldType  = String
type DefinitionType = String

data ConstValue = ConstNumber (Either Integer Double)
                | ConstLiteral String
                  deriving Show
