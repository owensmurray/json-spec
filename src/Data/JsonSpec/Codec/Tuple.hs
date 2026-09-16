{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  Description : Encode and decode via nested tuples

  Tuple-based interpretation of 'Specification'. The purpose of this
  module is to encode and decode Haskell values to and from Aeson
  'Value's, using a nested tuple structure as the intermediate
  representation.

  A type-level 'Specification' is translated into a canonical nested
  tuple type—an ordinary, inhabited Haskell type whose values you can
  construct and pattern-match on. Encoding and decoding then amount to
  converting between your domain types and that tuple structure, and
  between the tuple structure and an Aeson 'Value'.
-}
module Data.JsonSpec.Codec.Tuple (
  -- * Direct encoding/decoding
  {-|
    Encode or decode a value directly to or from an Aeson 'Value',
    given a 'Specification' and the corresponding tuple conversion.
  -}
  eitherDecode,
  encode,

  -- * Interacting with Aeson
  {-|
    'SpecJson' is the main way to plug a 'Specification' into Aeson's
    'ToJSON' / 'FromJSON' ecosystem (typically via @DerivingVia@).
  -}
  SpecJson(..),

  -- * Tuple encoding and decoding
  {-|
    'TupleEncoding' and 'TupleDecoding' convert between your Haskell
    types and the nested tuple structure that backs 'SpecJson' (and
    the direct encode/decode helpers above).
  -}
  TupleEncoding(..),
  TupleDecoding(..),
  Tag(..),
  Field(..),
  unField,
  Ref(..),

  -- * Other stuff
  {-|
    The items in this section are mainly exported because once in a
    while you might need to include them in a type signature, but they
    are not intended to be used directly.
  -}
  JsonStructure,
  StructureFromJson,
  StructureToJson,
) where

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import Data.JsonSpec.Codec.Tuple.Decode
  ( StructureFromJson(reprParseJson), TupleDecoding(fromJsonStructure)
  , eitherDecode
  )
import Data.JsonSpec.Codec.Tuple.Encode
  ( StructureToJson(reprToJson), TupleEncoding(toJsonStructure), encode
  )
import Data.JsonSpec.Codec.Tuple.Internal
  ( Field(Field), Ref(Ref, unRef), Tag(Tag), JsonStructure, unField
  )
import Data.JsonSpec.Spec
  ( HasJsonDecodingSpec(DecodingSpec), HasJsonEncodingSpec(EncodingSpec)
  )
import Prelude ((.), (<$>), (=<<))

{- |
  Helper for defining 'ToJSON' and 'FromJSON' instances based on
  'HasEncodingJsonSpec'.

  Use with -XDerivingVia like:

  > data MyObj = MyObj
  >   { foo :: Int
  >   , bar :: Text
  >   }
  >   deriving (ToJSON, FromJSON) via (SpecJson MyObj)
  > instance HasEncodingSpec MyObj where ...
  > instance HasDecodingSpec MyObj where ...
-}
newtype SpecJson a = SpecJson {unSpecJson :: a}
instance (StructureToJson (JsonStructure (EncodingSpec a)), TupleEncoding a) => ToJSON (SpecJson a) where
  toJSON = reprToJson . toJsonStructure . unSpecJson
instance (StructureFromJson (JsonStructure (DecodingSpec a)), TupleDecoding a) => FromJSON (SpecJson a) where
  parseJSON v =
    SpecJson <$>
      (fromJsonStructure =<< reprParseJson v)
