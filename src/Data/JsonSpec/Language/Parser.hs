{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
  Description : Parser for the JsonSpec textual language

  Megaparsec parser for the JsonSpec textual language.

  See @docs\/language-spec.md@. Trailing commas are allowed in
  objects (JSON-familiar). Identifiers accept letters matching
  'isAlpha'. Keyword names may be written as backtick-escaped
  identifiers (e.g. @`string`@).
-}
module Data.JsonSpec.Language.Parser (
  -- * AST
  Program(..),
  Binding(..),
  Spec(..),
  Field(..),

  -- * Parsing
  parseProgram,
  parseSpec,
  program,
  spec,
) where

import Control.Applicative
  ( Alternative((<|>), many), Applicative((<*), pure), (<$>), optional
  )
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import Data.Void (Void)
import Prelude
  ( Bool(False, True), Either(Left, Right), Enum(fromEnum, toEnum)
  , Eq((/=), (==)), Functor(fmap), Maybe(Just, Nothing), Monad((>>))
  , MonadFail(fail), Num((*), (+), (-)), Ord((<=), (>=)), Semigroup((<>)), ($)
  , (&&), (.), (||), Char, Int, Show, String, otherwise
  )
import Text.Megaparsec
  ( MonadParsec(eof, notFollowedBy, takeWhile1P, try), Parsec, between, choice
  , errorBundlePretty, manyTill, parse, satisfy, sepEndBy
  )
import Text.Megaparsec.Char (char, space1, string)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

{-| A full program: one top-level closed @module@ binding. -}
data Program = Program
  { programName :: Text
  , programSpec :: Spec
  }
  deriving stock (Eq, Show)


{-| A @type@ or @module@ binding inside a @let@. -}
data Binding
  = TypeBinding Text Spec
  | ModuleBinding Text Spec
  deriving stock (Eq, Show)


{-| A specification expression. -}
data Spec
  = LetSpec [Binding] Spec
  | EitherSpec [Spec]
  | DictSpec Spec
  | NullSpec Spec
  | StringSpec
  | NumberSpec
  | IntSpec
  | BoolSpec
  | DateTimeSpec
  | RawSpec
  | TagSpec Text
  | RefSpec Text
  | ObjectSpec [Field]
  | ArraySpec Spec
  deriving stock (Eq, Show)


{-| An object field. -}
data Field = Field
  { fieldName     :: Text
  , fieldOptional :: Bool
  , fieldSpec     :: Spec
  }
  deriving stock (Eq, Show)


type Parser = Parsec Void Text


{-| Parse a full program (@module Name = …@). -}
parseProgram
  :: String
  -> Text
  -> Either String Program
parseProgram name input =
  case parse (sc >> program <* eof) name input of
    Left err ->
      Left (errorBundlePretty err)
    Right p ->
      Right p


{-| Parse a bare specification expression (not a full program). -}
parseSpec
  :: String
  -> Text
  -> Either String Spec
parseSpec name input =
  case parse (sc >> spec <* eof) name input of
    Left err ->
      Left (errorBundlePretty err)
    Right s ->
      Right s


sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockCommentNested "{-" "-}")


lexeme :: Parser a -> Parser a
lexeme =
  L.lexeme sc


symbol :: Text -> Parser Text
symbol =
  L.symbol sc


{-| Top-level @module Name = spec@. -}
program :: Parser Program
program = do
  void (keyword "module")
  name <- ident
  void (symbol "=")
  body <- spec
  pure (Program name body)


{-| Parse a specification. -}
spec :: Parser Spec
spec =
  choice
    [ letSpec
    , eitherSpec
    , dictSpec
    , nullSpec
    , primary
    ]


letSpec :: Parser Spec
letSpec = do
  void (keyword "let")
  void (symbol "{")
  binds <- many binding
  void (keyword "in")
  body <- spec
  void (symbol "}")
  checkDuplicateBinds binds
  pure (LetSpec binds body)


binding :: Parser Binding
binding =
  typeBind <|> moduleBind


typeBind :: Parser Binding
typeBind = do
  void (keyword "type")
  name <- ident
  void (symbol "=")
  TypeBinding name <$> spec


moduleBind :: Parser Binding
moduleBind = do
  void (keyword "module")
  name <- ident
  void (symbol "=")
  ModuleBinding name <$> spec


eitherSpec :: Parser Spec
eitherSpec = do
  void (keyword "either")
  firstBranch <- eitherBranch
  rest <- many (try (symbol "|" >> primary))
  pure (EitherSpec (firstBranch : rest))


eitherBranch :: Parser Spec
eitherBranch = do
  _ <- optional (symbol "|")
  primary


dictSpec :: Parser Spec
dictSpec = do
  void (keyword "dict")
  DictSpec <$> primary


nullSpec :: Parser Spec
nullSpec = do
  void (keyword "null")
  NullSpec <$> primary


primary :: Parser Spec
primary =
  choice
    [ try (keyword "string")   >> pure StringSpec
    , try (keyword "number")   >> pure NumberSpec
    , try (keyword "int")      >> pure IntSpec
    , try (keyword "bool")     >> pure BoolSpec
    , try (keyword "datetime") >> pure DateTimeSpec
    , try (keyword "raw")      >> pure RawSpec
    , TagSpec <$> stringLit
    , RefSpec <$> ident
    , objectSpec
    , arraySpec
    , between (symbol "(") (symbol ")") spec
    ]


objectSpec :: Parser Spec
objectSpec = do
  void (symbol "{")
  fields <- field `sepEndBy` symbol ","
  void (symbol "}")
  checkDuplicateFields fields
  pure (ObjectSpec fields)


field :: Parser Field
field = do
  name <- stringLit
  opt <- optional (symbol "?")
  void (symbol ":")
  s <- spec
  pure Field
    { fieldName = name
    , fieldOptional = case opt of
        Just _ ->
          True
        Nothing ->
          False
    , fieldSpec = s
    }


arraySpec :: Parser Spec
arraySpec =
  ArraySpec <$> between (symbol "[") (symbol "]") spec


keywords :: Set.Set Text
keywords =
  Set.fromList
    [ "module", "type", "let", "in", "either", "dict", "null"
    , "string", "number", "int", "bool", "datetime", "raw"
    ]


keyword :: Text -> Parser ()
keyword w = lexeme . try $ do
  void (string w)
  notFollowedBy (satisfy identChar)


{-| Binding name or reference: bare non-keyword, or backtick-escaped. -}
ident :: Parser Text
ident =
  escapedIdent <|> bareIdent


{-| @`name`@ — may be a keyword (e.g. @`string`@, @`type`@). -}
escapedIdent :: Parser Text
escapedIdent = lexeme . try $ do
  void (char '`')
  name <- identBody
  void (char '`')
  pure name


{-| Bare identifier; keywords are rejected. -}
bareIdent :: Parser Text
bareIdent = lexeme . try $ do
  full <- identBody
  if Set.member full keywords then
    fail ("unexpected keyword " <> T.unpack full)
  else
    pure full


identBody :: Parser Text
identBody = do
  first <- takeWhile1P (Just "identifier") identCharStart
  rest <- fmap T.pack (many (satisfy identChar))
  pure (first <> rest)


identCharStart :: Char -> Bool
identCharStart c =
  isAlpha c || c == '_'


identChar :: Char -> Bool
identChar c =
  isAlphaNum c || c == '_'


stringLit :: Parser Text
stringLit = lexeme $ do
  void (char '"')
  chars <- manyTill stringChar (char '"')
  pure (T.pack chars)


stringChar :: Parser Char
stringChar =
  satisfy (\c -> c /= '"' && c /= '\\')
  <|> (char '\\' >> escape)


escape :: Parser Char
escape =
  choice
    [ char '"'  >> pure '"'
    , char '\\' >> pure '\\'
    , char '/'  >> pure '/'
    , char 'b'  >> pure '\b'
    , char 'f'  >> pure '\f'
    , char 'n'  >> pure '\n'
    , char 'r'  >> pure '\r'
    , char 't'  >> pure '\t'
    , char 'u'  >> unicodeEscape
    ]


unicodeEscape :: Parser Char
unicodeEscape = do
  d1 <- hexDigit
  d2 <- hexDigit
  d3 <- hexDigit
  d4 <- hexDigit
  pure (toEnum (d1 * 4096 + d2 * 256 + d3 * 16 + d4))


hexDigit :: Parser Int
hexDigit = do
  c <- satisfy isHex
  pure (hexVal c)


isHex :: Char -> Bool
isHex c =
  (c >= '0' && c <= '9')
  || (c >= 'a' && c <= 'f')
  || (c >= 'A' && c <= 'F')


hexVal :: Char -> Int
hexVal c
  | c >= '0' && c <= '9' =
      fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' =
      fromEnum c - fromEnum 'a' + 10
  | otherwise =
      fromEnum c - fromEnum 'A' + 10


checkDuplicateBinds :: [Binding] -> Parser ()
checkDuplicateBinds binds =
  checkDups "duplicate binding" (fmap bindName binds)


checkDuplicateFields :: [Field] -> Parser ()
checkDuplicateFields fields =
  checkDups "duplicate field" (fmap fieldName fields)


bindName :: Binding -> Text
bindName (TypeBinding n _) =
  n
bindName (ModuleBinding n _) =
  n


checkDups :: String -> [Text] -> Parser ()
checkDups msg names =
  go Set.empty names
  where
    go :: Set.Set Text -> [Text] -> Parser ()
    go _seen [] =
      pure ()
    go seen (n:ns)
      | Set.member n seen =
          fail (msg <> ": " <> T.unpack n)
      | otherwise =
          go (Set.insert n seen) ns
