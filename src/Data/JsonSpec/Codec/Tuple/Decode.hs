{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Tuple-structure decoding for specs. -}
module Data.JsonSpec.Codec.Tuple.Decode (
  StructureFromJson(..),
  TupleDecoding(..),
  eitherDecode,
) where

import Control.Applicative (Alternative((<|>)))
import Data.Aeson.Types
  ( FromJSON(parseJSON), Value(Null, Object), Parser, parseEither, withArray
  , withObject, withScientific, withText
  )
import Data.JsonSpec.Codec.Tuple.Internal
  ( Field(Field), Ref(Ref), Tag(Tag), JStruct, JsonStructure, sym
  )
import Data.JsonSpec.Spec (HasJsonDecodingSpec(DecodingSpec), Module)
import Data.Map (Map)
import Data.Proxy (Proxy)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.TypeLits (KnownSymbol)
import Prelude
  ( Applicative(pure), Either(Left, Right), Eq((==)), Functor(fmap)
  , Maybe(Just, Nothing), MonadFail(fail), Semigroup((<>))
  , Traversable(traverse), ($), (.), (<$>), Bool, Int, String
  )
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as Map
import qualified Data.Vector as Vector

{- |
  Decode a value from the structure appropriate for its specification.

  Given the structural encoding of the JSON data, parse the structure
  into the final type. The reason this returns a @'Parser' a@ instead of
  just a plain @a@ is because there may still be some invariants of the
  JSON data that the 'Specification' language is not able to express,
  and so you may need to fail parsing in those cases. For instance,
  'Specification' is not powerful enough to express "this field must
  contain only prime numbers".
-}
class (HasJsonDecodingSpec a) => TupleDecoding a where
  fromJsonStructure :: JsonStructure (DecodingSpec a) -> Parser a



{- |
  Analog of 'Data.Aeson.FromJSON', but specialized for decoding our
  "json representations", and closed to the user because the haskell
  representation scheme is fixed and not extensible by the user.

  We can't just use 'Data.Aeson.FromJSON' because the types we are using
  to represent "json data" (i.e. the 'JsonStructure' type family) already
  have 'ToJSON' instances. Even if we were to make a bunch of newtypes
  or whatever to act as the json representation (and therefor also force
  the user to do a lot of wrapping and unwrapping), that still wouldn't
  be sufficient because someone could always write an overlapping (or
  incoherent) 'ToJSON' instance of our newtype! This way we don't have
  to worry about any of that, and the types that the user must deal with
  when implementing 'fromJsonRepr' can be simple tuples and such.
-}
class StructureFromJson a where
  reprParseJson :: Value -> Parser a
instance StructureFromJson Value where
  reprParseJson = pure
instance StructureFromJson Text where
  reprParseJson = withText "string" pure
instance StructureFromJson Scientific where
  reprParseJson = withScientific "number" pure
instance StructureFromJson Int where
  reprParseJson = parseJSON
instance StructureFromJson () where
  reprParseJson =
    withObject "empty object" $ \_ -> pure ()
instance StructureFromJson Bool where
  reprParseJson = parseJSON
instance (KnownSymbol key, StructureFromJson val, StructureFromJson more) => StructureFromJson (Field key val, more) where
  reprParseJson =
    withObject "object" $ \o -> do
      more <- reprParseJson (Object o)
      case KM.lookup (sym @key) o of
        Nothing -> fail $ "could not find key: " <> sym @key
        Just rawVal -> do
          val <- reprParseJson rawVal
          pure (Field val, more)
instance (KnownSymbol key, StructureFromJson val, StructureFromJson more) => StructureFromJson (Maybe (Field key val), more) where
  reprParseJson =
    withObject "object" $ \o -> do
      more <- reprParseJson (Object o)
      case KM.lookup (sym @key) o of
        Nothing ->
          pure (Nothing, more)
        Just rawVal -> do
          val <- reprParseJson rawVal
          pure (Just (Field val), more)
instance (StructureFromJson left, StructureFromJson right) => StructureFromJson (Either left right) where
  reprParseJson v =
    (Left <$> reprParseJson v)
    <|> (Right <$> reprParseJson v)
instance (KnownSymbol const) => StructureFromJson (Tag const) where
  reprParseJson =
    withText "constant" $ \c ->
      if c == sym @const then pure Tag
      else fail "unexpected constant value"
instance (StructureFromJson a) => StructureFromJson [a] where
  reprParseJson =
    withArray
      "list"
      (fmap Vector.toList . traverse reprParseJson)
instance (StructureFromJson a) => StructureFromJson (Map Text a) where
  reprParseJson =
    withObject
      "dict"
      ( fmap Map.fromList
          . traverse
              ( \(key, val) ->
                  (\val_ -> (AK.toText key, val_)) <$> reprParseJson val
              )
          . KM.toList
      )
instance StructureFromJson UTCTime where
  reprParseJson = parseJSON
instance (StructureFromJson a) => StructureFromJson (Maybe a) where
  reprParseJson val = do
    case val of
      Null -> pure Nothing
      _ -> Just <$> reprParseJson val
instance
    (StructureFromJson (JStruct env spec))
  =>
    StructureFromJson (Ref env spec)
  where
  reprParseJson val =
    Ref <$> reprParseJson val


{-|
  Directly decode some JSON accoring to a spec without going through
  any To/FromJSON instances.
-}
eitherDecode
  :: forall spec.
     (StructureFromJson (JsonStructure spec))
   => Proxy (spec :: Module)
  -> Value
  -> Either String (JsonStructure spec)
eitherDecode _spec =
  parseEither reprParseJson
