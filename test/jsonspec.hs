{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (ByteString)
import Data.JsonSpec (Field(Field), HasJsonDecodingSpec(DecodingSpec,
  fromJSONStructure), HasJsonEncodingSpec(EncodingSpec, toJSONStructure),
  SpecJSON(SpecJSON), Specification(JsonDateTime, JsonEither, JsonInt,
  JsonNullable, JsonNum, JsonObject, JsonString, JsonTag), Tag(Tag))
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import Data.Time (UTCTime(UTCTime))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified Data.Aeson as A


main :: IO ()
main =
  hspec $ do
    describe "json" $ do
      it "encodes product" $
        let
          actual :: ByteString
          actual = A.encode $ sampleTestObject
          expected :: ByteString
          expected = "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":100}"
        in
          actual `shouldBe` expected

      it "decodes product" $
        let
          actual :: Either String TestObj
          actual =
            A.eitherDecode
              "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":100}"
          expected :: Either String TestObj
          expected = Right sampleTestObject
        in
          actual `shouldBe` expected

      it "encodes sum1" $
        let
          actual :: ByteString
          actual = A.encode $ TestA 0 "bar"
          expected :: ByteString
          expected = "{\"content\":{\"int-field\":0,\"txt-field\":\"bar\"},\"tag\":\"a\"}"
        in
          actual `shouldBe` expected

      it "encodes sum2" $
        let
          actual :: ByteString
          actual = A.encode $ TestB
          expected :: ByteString
          expected = "{\"tag\":\"b\"}"
        in
          actual `shouldBe` expected

      it "decodes sum1" $
        let
          actual :: Either String TestSum
          actual =
            A.eitherDecode
              "{\"content\":{\"int-field\":0,\"txt-field\":\"bar\"},\"tag\":\"a\"}"
          expected :: Either String TestSum
          expected = Right (TestA 0 "bar")
        in
          actual `shouldBe` expected

      it "decodes sum2" $
        let
          actual :: Either String TestSum
          actual = A.eitherDecode "{\"tag\":\"b\"}"
          expected :: Either String TestSum
          expected = Right TestB
        in
          actual `shouldBe` expected

      it "decodes UTCTime" $
        let
          actual :: Either String User
          actual =
            A.eitherDecode
              "{ \"name\": \"foo\", \"last-login\": \"1858-11-17T00:00:00Z\" }"
          
          expected :: Either String User
          expected =
            Right
              User
                { name = "foo"
                , lastLogin =
                    UTCTime (toEnum 0) 0
                }
        in
          actual `shouldBe` expected

      describe "nullable" $ do
        it "encodes product" $
          let
            actual :: ByteString
            actual = A.encode $ sampleTestObjectWithNull
            expected :: ByteString
            expected = "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":null}"
          in
            actual `shouldBe` expected

        it "decodes product" $
          let
            actual :: Either String TestObj
            actual =
              A.eitherDecode
                "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":null}"
            expected :: Either String TestObj
            expected = Right sampleTestObjectWithNull
          in
            actual `shouldBe` expected

      it "Bad tag does not decode" $
        let
          actual :: Either String TestSum
          actual = A.eitherDecode "{\"tag\":\"c\"}"
          expected :: Either String TestSum
          expected = Left "Error in $: unexpected constant value"
        in
          actual `shouldBe` expected


sampleTestObject :: TestObj
sampleTestObject =
  TestObj
    { foo = "foo"
    , bar = 1
    , baz = 
        TestSubObj
          { foo2 = "foo2"
          , bar2 = 0
          }

    , qux = Just 100
    }


sampleTestObjectWithNull:: TestObj
sampleTestObjectWithNull=
  TestObj
    { foo = "foo"
    , bar = 1
    , baz = 
        TestSubObj
          { foo2 = "foo2"
          , bar2 = 0
          }

    , qux = Nothing
    }


data TestSum
  = TestA Int Text
  | TestB
  deriving stock (Eq, Show)
  deriving ToJSON via (SpecJSON TestSum)
  deriving FromJSON via (SpecJSON TestSum)
instance HasJsonEncodingSpec TestSum where
  type EncodingSpec TestSum =
    JsonEither
      (JsonObject '[
        '("tag", JsonTag "a"),
        '("content", JsonObject [
          '("int-field", JsonNum),
          '("txt-field", JsonString)
        ])
      ])
      (JsonObject '[
        '("tag", JsonTag "b")
      ])
  toJSONStructure = \case
    TestA i t ->
      Left
        (Field @"tag" (Tag @"a"),
        (Field @"content"
          ( (Field @"int-field" (realToFrac i)
          , (Field @"txt-field" t
          , ()
          )
        )),
        ()))
    TestB ->
      Right
        ( Field @"tag" (Tag @"b")
        , ()
        )
instance HasJsonDecodingSpec TestSum where
  type DecodingSpec TestSum = EncodingSpec TestSum
  fromJSONStructure = \case
    Left (Field Tag, (Field (rawInt, (Field txt, ())), ())) -> do
      int <- parseInt rawInt
      pure (TestA int txt)
    Right _ ->
      pure TestB


data TestObj = TestObj
  { foo :: Text
  , bar :: Scientific
  , baz :: TestSubObj
  , qux :: Maybe Int
  }
  deriving stock (Show, Eq)
  deriving ToJSON via (SpecJSON TestObj)
  deriving FromJSON via (SpecJSON TestObj)
instance HasJsonEncodingSpec TestObj where
  type EncodingSpec TestObj =
    JsonObject
      '[
        '("foo", JsonString),
        '("bar", JsonNum),
        '("baz", EncodingSpec TestSubObj),
        '("qux", JsonNullable JsonInt)
      ]
  toJSONStructure TestObj { foo , bar , baz, qux } =
    (Field @"foo" foo,
    (Field @"bar" (realToFrac bar),
    (Field @"baz" (toJSONStructure baz),
    (Field @"qux" qux,
    ()))))
instance HasJsonDecodingSpec TestObj where
  type DecodingSpec TestObj = EncodingSpec TestObj
  fromJSONStructure
      (Field @"foo" foo,
      (Field @"bar" bar,
      (Field @"baz" rawBaz,
      (Field @"qux" qux,
      ()))))
    = do
      baz <- fromJSONStructure rawBaz
      pure $ TestObj { foo, bar, baz, qux }


data TestSubObj = TestSubObj
  { foo2 :: Text
  , bar2 :: Int
  }
  deriving stock (Show, Eq)
instance HasJsonEncodingSpec TestSubObj where
  type EncodingSpec TestSubObj =
    JsonObject
      '[
        '("foo", JsonString),
        '("bar", JsonNum)
      ]
  toJSONStructure TestSubObj { foo2 , bar2 } =
    (Field @"foo" foo2,
    (Field @"bar" (realToFrac bar2),
    ()))
instance HasJsonDecodingSpec TestSubObj where
  type DecodingSpec TestSubObj = EncodingSpec TestSubObj
  fromJSONStructure ((Field foo2), (rawBar, ())) = do
    bar2 <- parseInt rawBar
    pure TestSubObj {foo2 , bar2}


parseInt
  :: forall key.
     (KnownSymbol key)
  => Field key Scientific
  -> Parser Int
parseInt (Field val) =
  case floatingOrInteger val of
    Left (_ :: Float) ->
      fail $
        "Bad integer for property: "
        <> symbolVal (Proxy @key)
    Right i -> pure i


data User = User
  { name :: Text
  , lastLogin :: UTCTime
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON User)
instance HasJsonEncodingSpec User where
  type EncodingSpec User =
    JsonObject
      '[ '("name", JsonString)
       , '("last-login", JsonDateTime)
       ]
  toJSONStructure user =
    (Field @"name" (name user),
    (Field @"last-login" (lastLogin user),
    ()))
instance HasJsonDecodingSpec User where
  type DecodingSpec User = EncodingSpec User
  fromJSONStructure
      (Field @"name" name,
      (Field @"last-login" lastLogin,
      ()))
    =
      pure User { name , lastLogin }


