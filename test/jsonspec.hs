{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-
  Because of GHC-69797, we need to disable all warnings in order to
  disable the very specific warning about TypeAbstractions that can't
  be disabled individually, but then we re-enable the specific warnings
  we most care about.
-}
{-# OPTIONS_GHC -Wwarn #-}
{-# OPTIONS_GHC -Werror=missing-import-lists #-}

module Main (main) where

import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.JsonSpec
  ( Field(Field), FieldSpec(Optional, Required)
  , HasJsonDecodingSpec(DecodingSpec, fromJSONStructure)
  , HasJsonEncodingSpec(EncodingSpec, toJSONStructure), Rec(Rec, unRec)
  , SpecJSON(SpecJSON)
  , Specification
    ( JsonArray, JsonBool, JsonDateTime, JsonEither, JsonInt, JsonLet
    , JsonNullable, JsonNum, JsonObject, JsonRaw, JsonRef, JsonString, JsonTag
    )
  , Tag(Tag), (:::), (::?), eitherDecode, encode, unField
  )
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime(UTCTime))
import OM.Show (ShowJ(ShowJ))
import Prelude
  ( Applicative(pure), Bool(False, True), Either(Left, Right), Enum(toEnum)
  , Functor(fmap), Maybe(Just, Nothing), Monad((>>=)), Num(negate)
  , Traversable(traverse), ($), (.), Eq, IO, Int, Show, String, realToFrac
  )
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
          expected = "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qoo\":true,\"qux\":100}"
        in
          actual `shouldBe` expected

      it "decodes product" $
        let
          actual :: Either String TestObj
          actual =
            A.eitherDecode
              "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":100,\"qoo\":true}"
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

      describe "optionality" $ do
        let
          obj :: TestOptionality
          obj =
            TestOptionality
              { toFoo = Nothing
              , toBar = Nothing
              , toBaz = Nothing
              , toQux = 1
              }

        it "encodes" $
          let
            actual :: ByteString
            actual = A.encode obj

            expected :: ByteString
            expected = "{\"bar\":null,\"baz\":null,\"qux\":1}"
          in
            actual `shouldBe` expected

        it "decodes missing fields" $
          let
            actual :: Either String TestOptionality
            actual = A.eitherDecode "{\"bar\":null,\"qux\":1}"

            expected :: Either String TestOptionality
            expected = Right obj
          in
            actual `shouldBe` expected

        it "decodes explicit null" $
          let
            actual :: Either String TestOptionality
            actual = A.eitherDecode "{\"bar\":null,\"baz\":null,\"qux\":1}"

            expected :: Either String TestOptionality
            expected = Right obj
          in
            actual `shouldBe` expected

      describe "let" $ do
        it "decodes let" $
          let
            actual :: Either String Triangle
            actual =
              A.eitherDecode
                "{ \"vertex1\" : { \"x\": 1, \"y\": 2, \"z\": 3 }, \
                \  \"vertex2\" : { \"x\": 4, \"y\": 5, \"z\": 6 }, \
                \  \"vertex3\" : { \"x\": 7, \"y\": 8, \"z\": 9 } }"

            expected :: Either String Triangle
            expected =
              Right
                Triangle
                  { vertex1 = Vertex 1 2 3
                  , vertex2 = Vertex 4 5 6
                  , vertex3 = Vertex 7 8 9
                  }
          in
            actual `shouldBe` expected
        it "encodes let" $
            let
              actual :: ByteString
              actual =
                A.encode
                  Triangle
                    { vertex1 = Vertex 1 2 3
                    , vertex2 = Vertex 4 5 6
                    , vertex3 = Vertex 7 8 9
                    }

              expected :: ByteString
              expected = "{\"vertex1\":{\"x\":1,\"y\":2,\"z\":3},\"vertex2\":{\"x\":4,\"y\":5,\"z\":6},\"vertex3\":{\"x\":7,\"y\":8,\"z\":9}}"
            in
              actual `shouldBe` expected

      describe "recursive types" $ do
        it "decodes" $
          let
            actual :: Either String LabelledTree
            actual =
              A.eitherDecode
                "{\"children\":[{\"children\":[{\"children\":[],\"label\":\"child1\"},{\"children\":[],\"label\":\"child2\"}],\"label\":\"parent\"}],\"label\":\"grandparent\"}"

            expected :: Either String LabelledTree
            expected =
              Right
                LabelledTree
                  { label = "grandparent"
                  , children =
                      [ LabelledTree
                          { label = "parent"
                          , children =
                              [ LabelledTree
                                  { label = "child1"
                                  , children = []
                                  }
                              , LabelledTree
                                  { label = "child2"
                                  , children = []
                                  }
                              ]
                          }
                      ]
                  }
          in
            actual `shouldBe` expected
        it "decodes" $
          let
            actual :: ByteString
            actual =
              A.encode
                LabelledTree
                  { label = "grandparent"
                  , children =
                      [ LabelledTree
                          { label = "parent"
                          , children =
                              [ LabelledTree
                                  { label = "child1"
                                  , children = []
                                  }
                              , LabelledTree
                                  { label = "child2"
                                  , children = []
                                  }
                              ]
                          }
                      ]
                  }
            expected :: ByteString
            expected = "{\"children\":[{\"children\":[{\"children\":[],\"label\":\"child1\"},{\"children\":[],\"label\":\"child2\"}],\"label\":\"parent\"}],\"label\":\"grandparent\"}"
          in
            actual `shouldBe` expected

      describe "nullable" $ do
        it "encodes product" $
          let
            actual :: ByteString
            actual = A.encode $ sampleTestObjectWithNull
            expected :: ByteString
            expected = "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qoo\":false,\"qux\":null}"
          in
            actual `shouldBe` expected

        it "decodes product" $
          let
            actual :: Either String TestObj
            actual =
              A.eitherDecode
                "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":null,\"qoo\":false}"
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

      describe "direct encoding/decoding" $ do
        it "eitherDecode" $
          let
            actual
              :: Either
                   String
                   (Field "foo" Text,
                   (Maybe (Field "bar" Scientific),
                   (Field "baz"
                     (Field "foo" Text,
                     (Field "bar" Int,
                     ())),
                   (Field "qux" (Maybe Int),
                   (Field "qoo" Bool,
                   ())))))
            actual =
              A.eitherDecode
                "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":null,\"qoo\":false}"
                >>= eitherDecode (Proxy @(EncodingSpec TestObj))
            expected
              :: Either
                 String
                 (Field "foo" Text,
                 (Maybe (Field "bar" Scientific),
                 (Field "baz"
                   (Field "foo" Text,
                   (Field "bar" Int,
                   ())),
                 (Field "qux" (Maybe Int),
                 (Field "qoo" Bool,
                 ())))))
            expected =
              Right
                (Field @"foo" "foo",
                (Just (Field @"bar" 1.0),
                (Field @"baz"
                  (Field @"foo" "foo2",
                  (Field @"bar" 0,
                  ())),
                (Field @"qux" Nothing,
                (Field @"qoo" False,
                ())))))
          in
            actual `shouldBe` expected

        it "encode" $
          let
            expected :: Maybe A.Value
            expected =
              A.decode "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":null,\"qoo\":false}"

            actual :: Maybe A.Value
            actual =
              Just $
                encode
                  (Proxy @(EncodingSpec TestObj))
                  (
                    (Field @"foo" "foo",
                    (Just (Field @"bar" 1.0),
                    (Field @"baz"
                      (Field @"foo" "foo2",
                      (Field @"bar" 0,
                      ())),
                    (Field @"qux" Nothing,
                    (Field @"qoo" False,
                    ())))))
                  )
          in
            actual `shouldBe` expected

      describe "raw values" $ do
        it "decodes" $
          let
            expected :: Either String (Field "foo" A.Value, ())
            expected =
              Right
                (Field @"foo"
                  (
                    A.object
                      [ ("bar", A.String "barval")
                      , ("baz", A.toJSON [A.String "qux", A.Number 1.0, A.Bool False])
                      ]
                  )
                ,())

            actual :: Either String (Field "foo" A.Value, ())
            actual =
              A.eitherDecode
                "{ \"foo\": { \"bar\": \"barval\", \"baz\": [ \"qux\", 1, false ] } }"
              >>=
                eitherDecode (Proxy @( JsonObject '[ "foo" ::: JsonRaw ]))
          in
            actual `shouldBe` expected
        it "encodes" $
          let
            expected :: Maybe A.Value
            expected =
              A.decode
                "{ \"foo\": { \"bar\": \"barval\", \"baz\": [ \"qux\", 1, false ] } }"

            actual :: Maybe A.Value
            actual =
              Just $
                encode
                  (Proxy @( JsonObject '[ Required "foo" JsonRaw ]))
                  (Field @"foo"
                    (
                      A.object
                        [ ("bar", A.String "barval")
                        , ("baz", A.toJSON [A.String "qux", A.Number 1.0, A.Bool False])
                        ]
                    ),
                  ())
          in
            actual `shouldBe` expected

      it "HasField" $
        let
          expected :: Maybe TestHasField
          expected =
            Just
              TestHasField
                { thfFoo = "foo"
                , thfBar = 10
                , thfBaz =
                    TestSubObj
                      { foo2 = "bar"
                      , bar2 = negate 10
                      }
                }

          actual :: Maybe TestHasField
          actual =
            A.decode
              "{\
              \  \"foo\": \"foo\",\
              \  \"bar\": 10,\
              \  \"baz\": {\
              \    \"a_string\": \"bar\",\
              \    \"an_int\": -10\
              \  }\
              \}"
        in
          actual `shouldBe` expected


sampleTestObject :: TestObj
sampleTestObject =
  TestObj
    { foo = "foo"
    , bar = Just 1
    , baz =
        TestSubObj
          { foo2 = "foo2"
          , bar2 = 0
          }
    , qux = Just 100
    , qoo = True
    }


sampleTestObjectWithNull:: TestObj
sampleTestObjectWithNull=
  TestObj
    { foo = "foo"
    , bar = Just 1
    , baz =
        TestSubObj
          { foo2 = "foo2"
          , bar2 = 0
          }

    , qux = Nothing
    , qoo = False
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
        Required "tag" (JsonTag "a"),
        Required "content" (JsonObject [
          Required "int-field" JsonInt,
          Required "txt-field" JsonString
        ])
      ])
      (JsonObject '[
        Required "tag" (JsonTag "b")
      ])
  toJSONStructure = \case
    TestA i t ->
      Left
        (Field @"tag" (Tag @"a"),
        (Field @"content"
          ( (Field @"int-field" i
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
    Left
        (Field @"tag" Tag,
        (Field @"content"
          (Field @"int-field" int,
          (Field @"txt-field" txt,
          ())),
        ()))
      ->
        pure (TestA int txt)
    Right _ ->
      pure TestB


data TestObj = TestObj
  { foo :: Text
  , bar :: Maybe Scientific
  , baz :: TestSubObj
  , qux :: Maybe Int
  , qoo :: Bool
  }
  deriving stock (Show, Eq)
  deriving ToJSON via (SpecJSON TestObj)
  deriving FromJSON via (SpecJSON TestObj)
instance HasJsonEncodingSpec TestObj where
  type EncodingSpec TestObj =
    JsonObject
      '[
        Required "foo" JsonString,
        Optional "bar" JsonNum,
        Required "baz" (EncodingSpec TestSubObj),
        Required "qux" (JsonNullable JsonInt),
        Required "qoo" JsonBool
      ]
  toJSONStructure TestObj { foo , bar , baz, qux, qoo } =
    (Field @"foo" foo,
    (fmap (Field @"bar" . realToFrac) bar,
    (Field @"baz" (toJSONStructure baz),
    (Field @"qux" qux,
    (Field @"qoo" qoo,
    ())))))
instance HasJsonDecodingSpec TestObj where
  type DecodingSpec TestObj = EncodingSpec TestObj
  fromJSONStructure
      (Field @"foo" foo,
      (fmap (unField @"bar") -> bar,
      (Field @"baz" rawBaz,
      (Field @"qux" qux,
      (Field @"qoo" qoo,
      ())))))
    = do
      baz <- fromJSONStructure rawBaz
      pure TestObj { foo, bar, baz, qux, qoo }


data TestSubObj = TestSubObj
  { foo2 :: Text
  , bar2 :: Int
  }
  deriving stock (Show, Eq)
instance HasJsonEncodingSpec TestSubObj where
  type EncodingSpec TestSubObj =
    JsonObject
      '[ Required "foo" JsonString
       , Required "bar" JsonInt
       ]
  toJSONStructure TestSubObj { foo2 , bar2 } =
    (Field @"foo" foo2,
    (Field @"bar" bar2,
    ()))
instance HasJsonDecodingSpec TestSubObj where
  type DecodingSpec TestSubObj = EncodingSpec TestSubObj
  fromJSONStructure
      (Field @"foo" foo2,
      (Field @"bar" bar2,
      ()))
    =
      pure TestSubObj {foo2 , bar2}


data User = User
  { name :: Text
  , lastLogin :: UTCTime
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON User)
instance HasJsonEncodingSpec User where
  type EncodingSpec User =
    JsonObject
      '[ Required "name" JsonString
       , Required "last-login" JsonDateTime
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


data Vertex = Vertex
  { x :: Int
  , y :: Int
  , z :: Int
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON Vertex)
instance HasJsonEncodingSpec Vertex where
  type EncodingSpec Vertex =
    JsonObject
      '[ Required "x" JsonInt
       , Required "y" JsonInt
       , Required "z" JsonInt
       ]
  toJSONStructure Vertex {x, y, z} =
    (Field @"x" x,
    (Field @"y" y,
    (Field @"z" z,
    ())))
instance HasJsonDecodingSpec Vertex where
  type DecodingSpec Vertex = EncodingSpec Vertex
  fromJSONStructure
      (Field @"x" x,
      (Field @"y" y,
      (Field @"z" z,
      ())))
    =
      pure Vertex { x, y, z }


data Triangle = Triangle
  { vertex1 :: Vertex
  , vertex2 :: Vertex
  , vertex3 :: Vertex
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON Triangle)
instance HasJsonEncodingSpec Triangle where
  type EncodingSpec Triangle =
    JsonLet
      '[ '("Vertex", EncodingSpec Vertex) ]
      (JsonObject
        '[ Required "vertex1" (JsonRef "Vertex")
         , Required "vertex2" (JsonRef "Vertex")
         , Required "vertex3" (JsonRef "Vertex")
         ])
  toJSONStructure Triangle {vertex1, vertex2, vertex3} =
    (Field @"vertex1" (toJSONStructure vertex1),
    (Field @"vertex2" (toJSONStructure vertex2),
    (Field @"vertex3" (toJSONStructure vertex3),
    ())))
instance HasJsonDecodingSpec Triangle where
  type DecodingSpec Triangle = EncodingSpec Triangle
  fromJSONStructure
      (Field @"vertex1" rawVertex1,
      (Field @"vertex2" rawVertex2,
      (Field @"vertex3" rawVertex3,
      ())))
    = do
      vertex1 <- fromJSONStructure rawVertex1
      vertex2 <- fromJSONStructure rawVertex2
      vertex3 <- fromJSONStructure rawVertex3
      pure Triangle{vertex1, vertex2, vertex3}


data LabelledTree = LabelledTree
  {    label :: Text
  , children :: [LabelledTree]
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON LabelledTree)
instance HasJsonEncodingSpec LabelledTree where
  type EncodingSpec LabelledTree =
      JsonLet
        '[ '("LabelledTree",
               JsonObject
                 '[ Required "label" JsonString
                  , Required "children" (JsonArray (JsonRef "LabelledTree"))
                  ]
            )
         ]
        (JsonRef "LabelledTree")
  toJSONStructure LabelledTree {label , children } =
    (Field @"label" label,
    (Field @"children"
      [ Rec (toJSONStructure child)
      | child <- children
      ],
    ()))
instance HasJsonDecodingSpec LabelledTree where
  type DecodingSpec LabelledTree = EncodingSpec LabelledTree
  fromJSONStructure
      (Field @"label" label,
      (Field @"children" children_,
      ()))
    = do
      children <- traverse (fromJSONStructure . unRec) children_
      pure LabelledTree { label , children }


data TestOptionality = TestOptionality
  { toFoo :: Maybe Int
  , toBar :: Maybe Int
  , toBaz :: Maybe Int
  , toQux :: Int
  }
  deriving (ToJSON, FromJSON) via (SpecJSON TestOptionality)
  deriving (Show) via (ShowJ TestOptionality)
  deriving stock (Eq)
instance HasJsonEncodingSpec TestOptionality where
  type EncodingSpec TestOptionality =
    JsonObject
      '[ "foo" ::? JsonInt
       , Required "bar" (JsonNullable JsonInt)
       , Optional "baz" (JsonNullable JsonInt)
       , Required "qux" JsonInt
       ]

  toJSONStructure TestOptionality { toFoo , toBar , toBaz , toQux } =
    (fmap (Field @"foo") toFoo,
    (Field @"bar" toBar,
    ((Just . Field @"baz") toBaz, -- when encoding, prefer explicit null for testing.
    (Field @"qux" toQux,
    ()))))
instance HasJsonDecodingSpec TestOptionality where
  type DecodingSpec TestOptionality = EncodingSpec TestOptionality

  fromJSONStructure
      (fmap (unField @"foo") -> toFoo,
      (Field @"bar" toBar,
      (join . fmap (unField @"baz") -> toBaz,
      (Field @"qux" toQux,
      ()))))
    =
      pure TestOptionality { toFoo , toBar , toBaz , toQux }


data TestHasField = TestHasField
  { thfFoo :: Text
  , thfBar :: Int
  , thfBaz :: TestSubObj
  }
  deriving stock (Show, Eq)
  deriving (FromJSON) via (SpecJSON TestHasField)
instance HasJsonDecodingSpec TestHasField where
  type DecodingSpec TestHasField =
    JsonObject
      '[ "foo" ::: JsonString
       , "bar" ::: JsonInt
       , "baz" ::: JsonObject
                    '[ "a_string" ::: JsonString
                     ,   "an_int" ::: JsonInt
                     ]
       ]
  fromJSONStructure val =
    pure
      TestHasField
        { thfFoo = val.foo
        , thfBar = val.bar
        , thfBaz =
            TestSubObj
              { foo2 = val.baz.a_string
              , bar2 = val.baz.an_int
              }

        }


