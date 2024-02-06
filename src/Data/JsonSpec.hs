{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  This module provides a way to specify the shape of your JSON data at
  the type level.

  = Example

  > data User = User
  >   { name :: Text
  >   , lastLogin :: UTCTime
  >   }
  >   deriving stock (Show, Eq)
  >   deriving (ToJSON, FromJSON) via (SpecJSON User)
  > instance HasJsonEncodingSpec User where
  >   type EncodingSpec User =
  >     JsonObject
  >       '[ '("name", JsonString)
  >        , '("last-login", JsonDateTime)
  >        ]
  >   toJSONStructure user =
  >     (Field @"name" (name user),
  >     (Field @"last-login" (lastLogin user),
  >     ()))
  > instance HasJsonDecodingSpec User where
  >   type DecodingSpec User = EncodingSpec User
  >   fromJSONStructure
  >       (Field @"name" name,
  >       (Field @"last-login" lastLogin,
  >       ()))
  >     =
  >       pure User { name , lastLogin }

  = Motivation

  The particular use cases we focus on are enabling (but not providing
  in this package):

  1. Auto-generating documentation to ensure it is correct.
  2. Auto-generating client code in front-end languages to ensure it is correct.

  There are already tools available to achieve this, but they all have one
  major drawback: they rely on generically derived Aeson instances. Some
  people strongly object to using generically derived Aeson instances
  for encoding/decoding http api data because of how brittle it is. It
  can be surprisingly easy accidentally break your API without noticing
  because you don't realize that a small change to some type somewhere
  affects the API representation. Avoiding this requires very strict
  discipline about how you organize and maintain your code. E.g. you
  will see a lot of comments like

  > --| BEWARE, Changing any of the types in this file will change the API
  > -- representation!!
  > module My.API (...) where

  But then the types in this api might reference types in in other modules
  where it isn't as obvious that you might be changing the api when you
  make an update.

  I have even seen people go so far as to mandate that /every/ type
  appearing on the API must be in some similar \"API\" module. This
  usually ends badly because you end up with a bunch of seemingly spurious
  (and quite tedious) translations between between \"business\" types and
  almost identical \"API\" types.

  The other option is to simply not use generically derived instances
  and code all or some of your 'ToJSON'/'FromJSON' instances by hand. That
  (sometimes) helps solve the problem of making it a little more obvious
  when you are making a breaking api change. And it definitely helps
  with the ability to update the haskell type for some business purpose
  while keeping the encoding backwards compatible.

  The problem now though is that you can't take advantage of any of the
  above tooling without writing every instance by hand. Writing all the
  individual instances by hand defeat's the purpose because you are back
  to being unsure whether they are all in sync!

  The approach this library takes is to take a cue from `servant` and
  provide a way to specify the JSON encoding at the type level. You
  must manually specify the encoding, but you only have to do so once
  (at the type level). Other tools can then inspect the type using
  either type families or type classes to generate the appropriate
  artifacts or behavior. Aeson integration (provided by this package)
  works by using a type family to transform the spec into a new Haskell
  type whose structure is analogous to the specification. You are then
  required to transform your regular business value into a value of
  this ''structural type'' (I strongly recommend using type holes to
  make this easier). Values of the structural type will always encode
  into specification-complient JSON.
-}
module Data.JsonSpec (
  Specification(..),
  HasJsonEncodingSpec(..),
  HasJsonDecodingSpec(..),
  SpecJSON(..),
  Tag(..),
  Field(..),
  JSONStructure,
  Rec(..),
  eitherDecode,
  StructureFromJSON,
) where


import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import Data.JsonSpec.Decode (HasJsonDecodingSpec(DecodingSpec,
  fromJSONStructure), StructureFromJSON(reprParseJSON), eitherDecode)
import Data.JsonSpec.Encode (HasJsonEncodingSpec(EncodingSpec,
  toJSONStructure), StructureToJSON(reprToJSON))
import Data.JsonSpec.Spec (Field(Field), Rec(Rec, unRec),
  Specification(JsonArray, JsonBool, JsonDateTime, JsonEither, JsonInt,
  JsonLet, JsonNullable, JsonNum, JsonObject, JsonRef, JsonString,
  JsonTag), Tag(Tag), JSONStructure)
import Prelude ((.), (<$>), (=<<))


{- |
  Helper for defining 'ToJSON' and 'FromJSON' instances based on
  'HasEncodingJsonSpec'.

  Use with -XDerivingVia like:

  > data MyObj = MyObj
  >   { foo :: Int
  >   , bar :: Text
  >   }
  >   deriving (ToJSON, FromJSON) via (SpecJSON MyObj)
  > instance HasEncodingSpec MyObj where ...
  > instance HasDecodingSpec MyObj where ...
-}
newtype SpecJSON a = SpecJSON {unSpecJson :: a}
instance (StructureToJSON (JSONStructure (EncodingSpec a)), HasJsonEncodingSpec a) => ToJSON (SpecJSON a) where
  toJSON = reprToJSON . toJSONStructure . unSpecJson
instance (StructureFromJSON (JSONStructure (DecodingSpec a)), HasJsonDecodingSpec a) => FromJSON (SpecJSON a) where
  parseJSON v =
    SpecJSON <$>
      (fromJSONStructure =<< reprParseJSON v)


