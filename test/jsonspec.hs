{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
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
{-# OPTIONS_GHC -Werror=missing-import-lists #-}

module Main (main) where

import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Either (isLeft)
import Data.JsonSpec
  ( BindingSpec(ModuleBind, TypeBind), FieldSpec(Optional, Required)
  , HasJsonDecodingSpec(DecodingSpec), HasJsonEncodingSpec(EncodingSpec)
  , Module(Module)
  , Specification
    ( JsonAnnotated, JsonArray, JsonBool, JsonDateTime, JsonDict, JsonEither
    , JsonInt, JsonLet, JsonModule, JsonNullable, JsonNum, JsonObject, JsonRaw
    , JsonRef, JsonString, JsonTag
    )
  , type (:::), type (::?)
  )
import Data.JsonSpec.Codec.Tuple
  ( Field(Field), Ref(Ref), SpecJson(SpecJson), Tag(Tag)
  , TupleDecoding(fromJsonStructure), TupleEncoding(toJsonStructure)
  , eitherDecode, encode, unField
  )
import Data.Map (Map)
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
import Test.Hspec (describe, hspec, it, shouldBe, shouldSatisfy)
import qualified Data.Aeson as A
import qualified Data.Map as Map

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

      describe "dict" $ do
        it "encodes an empty dict" $
          let
            actual :: A.Value
            actual =
              encode
                (Proxy @('Module (JsonDict JsonInt)))
                Map.empty

            expected :: A.Value
            expected = A.object []
          in
            actual `shouldBe` expected

        it "round-trips a non-empty int dict" $
          let
            raw :: A.Value
            raw =
              A.object
                [ ("alpha", A.Number 1)
                , ("beta", A.Number 2)
                ]

            decoded :: Either String (Map Text Int)
            decoded =
              eitherDecode
                (Proxy @('Module (JsonDict JsonInt)))
                raw

            encoded :: Either String A.Value
            encoded =
              fmap
                (encode (Proxy @('Module (JsonDict JsonInt))))
                decoded

            expected :: Either String A.Value
            expected = Right raw
          in
            encoded `shouldBe` expected

        it "decodes a dict of objects" $
          let
            raw :: A.Value
            raw =
              A.object
                [ ( "first"
                  , A.object
                      [ ("foo", A.String "first")
                      , ("bar", A.Number 1)
                      ]
                  )
                , ( "second"
                  , A.object
                      [ ("foo", A.String "second")
                      , ("bar", A.Number 2)
                      ]
                  )
                ]

            actual
              :: Either
                   String
                   (Map Text
                     (Field "foo" Text,
                     (Field "bar" Int,
                     ())))
            actual =
              eitherDecode
                (Proxy @('Module (JsonDict (JsonObject
                  '[ "foo" ::: JsonString
                   , "bar" ::: JsonInt
                   ]))))
                raw

            expected
              :: Either
                   String
                   (Map Text
                     (Field "foo" Text,
                     (Field "bar" Int,
                     ())))
            expected =
              Right $
                Map.fromList
                  [ ( "first"
                    , (Field @"foo" "first",
                      (Field @"bar" 1,
                      ()))
                    )
                  , ( "second"
                    , (Field @"foo" "second",
                      (Field @"bar" 2,
                      ()))
                    )
                  ]
          in
            actual `shouldBe` expected

        it "decodes a dict of nullable values" $
          let
            raw :: A.Value
            raw =
              A.object
                [ ("present", A.String "value")
                , ("missing", A.Null)
                ]

            actual :: Either String (Map Text (Maybe Text))
            actual =
              eitherDecode
                (Proxy @('Module (JsonDict (JsonNullable JsonString))))
                raw

            expected :: Either String (Map Text (Maybe Text))
            expected =
              Right $
                Map.fromList
                  [ ("missing", Nothing)
                  , ("present", Just "value")
                  ]
          in
            actual `shouldBe` expected

        it "rejects dict values that do not match the value spec" $
          let
            actual :: Either String (Map Text Int)
            actual =
              eitherDecode
                (Proxy @('Module (JsonDict JsonInt)))
                (A.object [("bad", A.String "not an int")])
          in
            actual `shouldSatisfy` isLeft

        it "rejects non-object dict JSON" $
          let
            actual :: Either String (Map Text Int)
            actual =
              eitherDecode
                (Proxy @('Module (JsonDict JsonInt)))
                (A.String "not an object")
          in
            actual `shouldSatisfy` isLeft

        it "works inside an object field" $
          let
            actual
              :: Either
                   String
                   (Field "attrs" (Map Text Int), ())
            actual =
              eitherDecode
                (Proxy @('Module (JsonObject '[ "attrs" ::: JsonDict JsonInt ])))
                ( A.object
                    [ ( "attrs"
                      , A.object
                          [ ("alpha", A.Number 1)
                          , ("beta", A.Number 2)
                          ]
                      )
                    ]
                )

            expected
              :: Either
                   String
                   (Field "attrs" (Map Text Int), ())
            expected =
              Right
                ( Field @"attrs" $
                    Map.fromList
                      [ ("alpha", 1)
                      , ("beta", 2)
                      ]
                , ()
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
                eitherDecode (Proxy @('Module (JsonObject '[ "foo" ::: JsonRaw ])))
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
                  (Proxy @('Module (JsonObject '[ Required "foo" JsonRaw ])))
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

      describe "HasField" $ do
        it "Basic HasField" $
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

        it "missing optional fields" $
          let
            expected :: Maybe TestOptionalHasField
            expected =
              Just
                TestOptionalHasField
                  { foo = Nothing
                  , bar = Nothing
                  }

            actual :: Maybe TestOptionalHasField
            actual = A.decode "{}"
          in
            actual `shouldBe` expected

        it "supplied optional fields" $
          let
            expected :: Maybe TestOptionalHasField
            expected =
              Just
                TestOptionalHasField
                  { foo = Just "foo"
                  , bar = Just Nothing
                  }

            actual :: Maybe TestOptionalHasField
            actual = A.decode "{\"foo\": \"foo\", \"bar\": null}"
          in
            actual `shouldBe` expected
        it "mixed optional fields" $
          let
            expected :: Maybe TestOptionalHasField
            expected =
              Just
                TestOptionalHasField
                  { foo = Nothing
                  , bar = Just (Just "bar")
                  }

            actual :: Maybe TestOptionalHasField
            actual = A.decode "{\"bar\": \"bar\"}"
          in
            actual `shouldBe` expected

      describe "annotated" $ do
        it "encodes" $
          let
            actual :: ByteString
            actual =
              A.encode
                AnnotatedUser
                  { auName = "alice"
                  , auAge = 30
                  }

            expected :: ByteString
            expected = "{\"age\":30,\"name\":\"alice\"}"
          in
            actual `shouldBe` expected

        it "decodes" $
          let
            actual :: Either String AnnotatedUser
            actual = A.eitherDecode "{\"name\":\"alice\",\"age\":30}"

            expected :: Either String AnnotatedUser
            expected =
              Right
                AnnotatedUser
                  { auName = "alice"
                  , auAge = 30
                  }
          in
            actual `shouldBe` expected

        it "works with JsonLet" $
          let
            actual :: ByteString
            actual =
              A.encode
                AnnotatedTriangle
                  { atVertex1 = AnnotatedVertex 1 2 3
                  , atVertex2 = AnnotatedVertex 4 5 6
                  , atVertex3 = AnnotatedVertex 7 8 9
                  }

            expected :: ByteString
            expected = "{\"vertex1\":{\"x\":1,\"y\":2,\"z\":3},\"vertex2\":{\"x\":4,\"y\":5,\"z\":6},\"vertex3\":{\"x\":7,\"y\":8,\"z\":9}}"
          in
            actual `shouldBe` expected

        it "decodes with JsonLet" $
          let
            actual :: Either String AnnotatedTriangle
            actual =
              A.eitherDecode
                "{\"vertex1\":{\"x\":1,\"y\":2,\"z\":3},\"vertex2\":{\"x\":4,\"y\":5,\"z\":6},\"vertex3\":{\"x\":7,\"y\":8,\"z\":9}}"

            expected :: Either String AnnotatedTriangle
            expected =
              Right
                AnnotatedTriangle
                  { atVertex1 = AnnotatedVertex 1 2 3
                  , atVertex2 = AnnotatedVertex 4 5 6
                  , atVertex3 = AnnotatedVertex 7 8 9
                  }
          in
            actual `shouldBe` expected

        it "supports non-Symbol annotation values (e.g. Bool)" $
          let
            actual :: ByteString
            actual =
              A.encode
                AnnotatedWithBool
                  { awbName = "test"
                  }

            expected :: ByteString
            expected = "{\"name\":\"test\"}"
          in
            actual `shouldBe` expected

      describe "mutual recursion" $ do
        describe "style1" $ do
          it "encodes" $
            let
              expected :: ByteString
              expected = "[[[],[]]]"

              actual :: ByteString
              actual = A.encode (MRec1 [MRec2 [MRec1 [], MRec1 []]])
            in
              actual `shouldBe` expected

          it "decoces" $
            let
              expected :: Maybe MRec1
              expected = Just (MRec1 [MRec2 [MRec1 [], MRec1 []]])

              actual :: Maybe MRec1
              actual = A.decode "[[[],[]]]"
            in
              actual `shouldBe` expected

        describe "style2" $ do
          it "encodes" $
            let
              expected :: ByteString
              expected =
                "{\"foo\":{\"bar\":{\"foo\":{\"bar\":{\"foo\":null}}}}}"

              actual =
                A.encode
                  MRec3
                    { foo =
                        Just
                          MRec4
                            { bar =
                                MRec3
                                  { foo =
                                      Just
                                        MRec4
                                          { bar =
                                              MRec3
                                                { foo = Nothing
                                                }
                                          }
                                  }
                            }
                    }
            in
              actual `shouldBe` expected

          it "decodes" $
            let
              expected :: Maybe MRec3
              expected =
                Just
                  MRec3
                    { foo =
                        Just
                          MRec4
                            { bar =
                                MRec3
                                  { foo =
                                      Just
                                        MRec4
                                          { bar =
                                              MRec3
                                                { foo = Nothing
                                                }
                                          }
                                  }
                            }
                    }

              actual :: Maybe MRec3
              actual =
                A.decode
                  "{\"foo\":{\"bar\":{\"foo\":{\"bar\":{\"foo\":null}}}}}"
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
  deriving ToJSON via (SpecJson TestSum)
  deriving FromJSON via (SpecJson TestSum)
instance HasJsonEncodingSpec TestSum where
  type EncodingSpec TestSum =
    'Module
      (JsonEither
      '[
        JsonObject '[
          Required "tag" (JsonTag "a"),
          Required "content" (JsonObject [
            Required "int-field" JsonInt,
            Required "txt-field" JsonString
          ])
        ],
        JsonObject '[
          Required "tag" (JsonTag "b")
        ]
      ])
instance TupleEncoding TestSum where
  toJsonStructure = \case
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
instance TupleDecoding TestSum where
  fromJsonStructure = \case
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


data TestOptionalHasField = TestOptionalHasField
  { foo :: Maybe Text
  , bar :: Maybe (Maybe Text)
  }
  deriving stock (Show, Eq)
  deriving FromJSON via (SpecJson TestOptionalHasField)
instance HasJsonDecodingSpec TestOptionalHasField where
  type DecodingSpec TestOptionalHasField =
    'Module
      (JsonObject
        '[ "foo" ::? JsonString
         , "bar" ::? JsonNullable JsonString
         ])
instance TupleDecoding TestOptionalHasField where
  fromJsonStructure v =
    pure
      TestOptionalHasField
        { foo = v.foo
        , bar = v.bar
        }


data TestObj = TestObj
  { foo :: Text
  , bar :: Maybe Scientific
  , baz :: TestSubObj
  , qux :: Maybe Int
  , qoo :: Bool
  }
  deriving stock (Show, Eq)
  deriving ToJSON via (SpecJson TestObj)
  deriving FromJSON via (SpecJson TestObj)
instance HasJsonEncodingSpec TestObj where
  type EncodingSpec TestObj =
    'Module
      (JsonObject
      '[
        Required "foo" JsonString,
        Optional "bar" JsonNum,
        Required "baz" (JsonModule (EncodingSpec TestSubObj)),
        Required "qux" (JsonNullable JsonInt),
        Required "qoo" JsonBool
      ])
instance TupleEncoding TestObj where
  toJsonStructure TestObj { foo , bar , baz, qux, qoo } =
    (Field @"foo" foo,
    (fmap (Field @"bar" . realToFrac) bar,
    (Field @"baz" (toJsonStructure baz),
    (Field @"qux" qux,
    (Field @"qoo" qoo,
    ())))))
instance HasJsonDecodingSpec TestObj where
  type DecodingSpec TestObj = EncodingSpec TestObj
instance TupleDecoding TestObj where
  fromJsonStructure
      (Field @"foo" foo,
      (fmap (unField @"bar") -> bar,
      (Field @"baz" rawBaz,
      (Field @"qux" qux,
      (Field @"qoo" qoo,
      ())))))
    = do
      baz <- fromJsonStructure rawBaz
      pure TestObj { foo, bar, baz, qux, qoo }


data TestSubObj = TestSubObj
  { foo2 :: Text
  , bar2 :: Int
  }
  deriving stock (Show, Eq)
instance HasJsonEncodingSpec TestSubObj where
  type EncodingSpec TestSubObj =
    'Module
      (JsonObject
      '[ Required "foo" JsonString
       , Required "bar" JsonInt
       ])
instance TupleEncoding TestSubObj where
  toJsonStructure TestSubObj { foo2 , bar2 } =
    (Field @"foo" foo2,
    (Field @"bar" bar2,
    ()))
instance HasJsonDecodingSpec TestSubObj where
  type DecodingSpec TestSubObj = EncodingSpec TestSubObj
instance TupleDecoding TestSubObj where
  fromJsonStructure
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
  deriving (ToJSON, FromJSON) via (SpecJson User)
instance HasJsonEncodingSpec User where
  type EncodingSpec User =
    'Module
      (JsonObject
      '[ Required "name" JsonString
       , Required "last-login" JsonDateTime
       ])
instance TupleEncoding User where
  toJsonStructure user =
    (Field @"name" (name user),
    (Field @"last-login" (lastLogin user),
    ()))
instance HasJsonDecodingSpec User where
  type DecodingSpec User = EncodingSpec User
instance TupleDecoding User where
  fromJsonStructure
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
  deriving (ToJSON, FromJSON) via (SpecJson Vertex)
instance HasJsonEncodingSpec Vertex where
  type EncodingSpec Vertex =
    'Module
      (JsonObject
      '[ Required "x" JsonInt
       , Required "y" JsonInt
       , Required "z" JsonInt
       ])
instance TupleEncoding Vertex where
  toJsonStructure Vertex {x, y, z} =
    (Field @"x" x,
    (Field @"y" y,
    (Field @"z" z,
    ())))
instance HasJsonDecodingSpec Vertex where
  type DecodingSpec Vertex = EncodingSpec Vertex
instance TupleDecoding Vertex where
  fromJsonStructure
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
  deriving (ToJSON, FromJSON) via (SpecJson Triangle)
instance HasJsonEncodingSpec Triangle where
  type EncodingSpec Triangle =
    'Module
      (JsonLet
      '[ ModuleBind "Vertex" (EncodingSpec Vertex) ]
      (JsonObject
        '[ Required "vertex1" (JsonRef "Vertex")
         , Required "vertex2" (JsonRef "Vertex")
         , Required "vertex3" (JsonRef "Vertex")
         ]))
instance TupleEncoding Triangle where
  toJsonStructure Triangle {vertex1, vertex2, vertex3} =
    (Field @"vertex1" (Ref $ toJsonStructure vertex1),
    (Field @"vertex2" (Ref $ toJsonStructure vertex2),
    (Field @"vertex3" (Ref $ toJsonStructure vertex3),
    ())))
instance HasJsonDecodingSpec Triangle where
  type DecodingSpec Triangle = EncodingSpec Triangle
instance TupleDecoding Triangle where
  fromJsonStructure
      (Field @"vertex1" (Ref rawVertex1),
      (Field @"vertex2" (Ref rawVertex2),
      (Field @"vertex3" (Ref rawVertex3),
      ())))
    = do
      vertex1 <- fromJsonStructure rawVertex1
      vertex2 <- fromJsonStructure rawVertex2
      vertex3 <- fromJsonStructure rawVertex3
      pure Triangle{vertex1, vertex2, vertex3}


data LabelledTree = LabelledTree
  {    label :: Text
  , children :: [LabelledTree]
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJson LabelledTree)
instance HasJsonEncodingSpec LabelledTree where
  type EncodingSpec LabelledTree =
    'Module
      (JsonLet
        '[ TypeBind "LabelledTree"
             (JsonObject
               '[ Required "label" JsonString
                , Required "children" (JsonArray (JsonRef "LabelledTree"))
                ]
             )
         ]
        (JsonRef "LabelledTree"))
instance TupleEncoding LabelledTree where
  toJsonStructure LabelledTree {label , children } =
    Ref
      (Field @"label" label,
      (Field @"children"
        [ toJsonStructure child
        | child <- children
        ],
      ()))
instance HasJsonDecodingSpec LabelledTree where
  type DecodingSpec LabelledTree = EncodingSpec LabelledTree
instance TupleDecoding LabelledTree where
  fromJsonStructure
      (
        Ref
          (Field @"label" label,
          (Field @"children" children_,
          ()))
      )
    = do
      children <- traverse fromJsonStructure children_
      pure LabelledTree { label , children }


data TestOptionality = TestOptionality
  { toFoo :: Maybe Int
  , toBar :: Maybe Int
  , toBaz :: Maybe Int
  , toQux :: Int
  }
  deriving (ToJSON, FromJSON) via (SpecJson TestOptionality)
  deriving (Show) via (ShowJ TestOptionality)
  deriving stock (Eq)
instance HasJsonEncodingSpec TestOptionality where
  type EncodingSpec TestOptionality =
    'Module
      (JsonObject
      '[ "foo" ::? JsonInt
       , Required "bar" (JsonNullable JsonInt)
       , Optional "baz" (JsonNullable JsonInt)
       , Required "qux" JsonInt
       ])
instance TupleEncoding TestOptionality where
  toJsonStructure TestOptionality { toFoo , toBar , toBaz , toQux } =
    (fmap (Field @"foo") toFoo,
    (Field @"bar" toBar,
    ((Just . Field @"baz") toBaz, -- when encoding, prefer explicit null for testing.
    (Field @"qux" toQux,
    ()))))
instance HasJsonDecodingSpec TestOptionality where
  type DecodingSpec TestOptionality = EncodingSpec TestOptionality
instance TupleDecoding TestOptionality where
  fromJsonStructure
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
  deriving (FromJSON) via (SpecJson TestHasField)
instance HasJsonDecodingSpec TestHasField where
  type DecodingSpec TestHasField =
    'Module
      (JsonObject
        '[ "foo" ::: JsonString
         , "bar" ::: JsonInt
         , "baz" ::: JsonObject
                       '[ "a_string" ::: JsonString
                        ,   "an_int" ::: JsonInt
                        ]
         ])
instance TupleDecoding TestHasField where
  fromJsonStructure val =
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


{- Mutually recursive test.  -}
{- ========================================================================== -}

newtype MRec1 = MRec1 [MRec2]
  deriving (ToJSON, FromJSON) via (SpecJson MRec1)
  deriving stock (Show, Eq)
newtype MRec2 = MRec2 [MRec1]
  deriving stock (Show, Eq)
instance HasJsonEncodingSpec MRec1 where
  type EncodingSpec MRec1 =
    'Module
      (JsonLet
     '[ TypeBind "one" (JsonArray (JsonRef "two"))
      , TypeBind "two" (JsonArray (JsonRef "one"))
      ]
      (JsonRef "one"))
instance TupleEncoding MRec1 where
  toJsonStructure (MRec1 m2s) =
    Ref
      [ Ref (fmap toJsonStructure m1s)
      | MRec2 m1s <- m2s
      ]
instance HasJsonDecodingSpec MRec1 where
  type DecodingSpec MRec1 = EncodingSpec MRec1
instance TupleDecoding MRec1 where
  fromJsonStructure (Ref m2s_) = do
    m2s <-
      traverse
        (\(Ref m1s_) -> do
          m1s <- traverse fromJsonStructure m1s_
          pure (MRec2 m1s)
        )
        m2s_
    pure (MRec1 m2s)


{- Another mutually recursive test. -}
{- ========================================================================== -}

type SharedRecSpecs =
  '[ TypeBind "three"
       (JsonObject
         '[ "foo" ::: JsonNullable (JsonRef "four")
          ]
       )
   , TypeBind "four"
       (JsonObject
         '[ "bar" ::: JsonRef "three"
          ]
       )
   ]


newtype MRec3 = MRec3
  { foo :: Maybe MRec4
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJson MRec3)
instance HasJsonEncodingSpec MRec3 where
  type EncodingSpec MRec3 =
    'Module
      (JsonLet SharedRecSpecs (JsonRef "three"))
instance TupleEncoding MRec3 where
  toJsonStructure MRec3 { foo } =
    Ref
      (Field @"foo" (fmap toJsonStructure foo),
      ())
instance HasJsonDecodingSpec MRec3 where
  type DecodingSpec MRec3 = EncodingSpec MRec3
instance TupleDecoding MRec3 where
  fromJsonStructure ( Ref (Field @"foo" rawFoo, ()))
    = do
      foo <- traverse fromJsonStructure rawFoo
      pure MRec3 { foo }


newtype MRec4 = MRec4
  { bar :: MRec3
  }
  deriving stock (Show, Eq)
instance HasJsonEncodingSpec MRec4 where
  type EncodingSpec MRec4 =
    'Module
      (JsonLet SharedRecSpecs (JsonRef "four"))
instance TupleEncoding MRec4 where
  toJsonStructure MRec4 { bar } =
    Ref
      (Field @"bar" (toJsonStructure bar),
      ())
instance HasJsonDecodingSpec MRec4 where
  type DecodingSpec MRec4 = EncodingSpec MRec4
instance TupleDecoding MRec4 where
  fromJsonStructure ( Ref (Field @"bar" rawbar, ()))
    = do
      bar <- fromJsonStructure rawbar
      pure MRec4 { bar }

{- ========================================================================== -}


{- Annotated test. -}
{- ========================================================================== -}

data AnnotatedUser = AnnotatedUser
  { auName :: Text
  ,  auAge :: Int
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJson AnnotatedUser)
instance HasJsonEncodingSpec AnnotatedUser where
  type EncodingSpec AnnotatedUser =
    'Module
      (JsonAnnotated
      '[ '("description", "A user with a name and age")
       , '("example", "{\"name\": \"alice\", \"age\": 30}")
       ]
      (JsonObject
        '[ Required "name" JsonString
         , Required "age" JsonInt
         ]))
instance TupleEncoding AnnotatedUser where
  toJsonStructure AnnotatedUser { auName, auAge } =
    (Field @"name" auName,
    (Field @"age" auAge,
    ()))
instance HasJsonDecodingSpec AnnotatedUser where
  type DecodingSpec AnnotatedUser = EncodingSpec AnnotatedUser
instance TupleDecoding AnnotatedUser where
  fromJsonStructure
      (Field @"name" auName,
      (Field @"age" auAge,
      ()))
    =
      pure AnnotatedUser { auName, auAge }


data AnnotatedVertex = AnnotatedVertex
  { avX :: Int
  , avY :: Int
  , avZ :: Int
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJson AnnotatedVertex)
instance HasJsonEncodingSpec AnnotatedVertex where
  type EncodingSpec AnnotatedVertex =
    'Module
      (JsonAnnotated
      '[ '("description", "A 3D vertex") ]
      (JsonObject
        '[ Required "x" JsonInt
         , Required "y" JsonInt
         , Required "z" JsonInt
         ]))
instance TupleEncoding AnnotatedVertex where
  toJsonStructure AnnotatedVertex { avX, avY, avZ } =
    (Field @"x" avX,
    (Field @"y" avY,
    (Field @"z" avZ,
    ())))
instance HasJsonDecodingSpec AnnotatedVertex where
  type DecodingSpec AnnotatedVertex = EncodingSpec AnnotatedVertex
instance TupleDecoding AnnotatedVertex where
  fromJsonStructure
      (Field @"x" avX,
      (Field @"y" avY,
      (Field @"z" avZ,
      ())))
    =
      pure AnnotatedVertex { avX, avY, avZ }


data AnnotatedTriangle = AnnotatedTriangle
  { atVertex1 :: AnnotatedVertex
  , atVertex2 :: AnnotatedVertex
  , atVertex3 :: AnnotatedVertex
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJson AnnotatedTriangle)
instance HasJsonEncodingSpec AnnotatedTriangle where
  type EncodingSpec AnnotatedTriangle =
    'Module
      (JsonLet
      '[ TypeBind "Vertex"
           (JsonAnnotated
             '[ '("description", "A 3D vertex used in shapes") ]
             (JsonObject
               '[ Required "x" JsonInt
                , Required "y" JsonInt
                , Required "z" JsonInt
                ]))
       ]
      (JsonAnnotated
        '[ '("description", "A triangle with three vertices") ]
        (JsonObject
          '[ Required "vertex1" (JsonRef "Vertex")
           , Required "vertex2" (JsonRef "Vertex")
           , Required "vertex3" (JsonRef "Vertex")
           ])))
instance TupleEncoding AnnotatedTriangle where
  toJsonStructure AnnotatedTriangle { atVertex1, atVertex2, atVertex3 } =
    (Field @"vertex1" (Ref $ toJsonStructure atVertex1),
    (Field @"vertex2" (Ref $ toJsonStructure atVertex2),
    (Field @"vertex3" (Ref $ toJsonStructure atVertex3),
    ())))
instance HasJsonDecodingSpec AnnotatedTriangle where
  type DecodingSpec AnnotatedTriangle = EncodingSpec AnnotatedTriangle
instance TupleDecoding AnnotatedTriangle where
  fromJsonStructure
      (Field @"vertex1" (Ref rawVertex1),
      (Field @"vertex2" (Ref rawVertex2),
      (Field @"vertex3" (Ref rawVertex3),
      ())))
    = do
      atVertex1 <- fromJsonStructure rawVertex1
      atVertex2 <- fromJsonStructure rawVertex2
      atVertex3 <- fromJsonStructure rawVertex3
      pure AnnotatedTriangle { atVertex1, atVertex2, atVertex3 }


data AnnotatedWithBool = AnnotatedWithBool
  { awbName :: Text
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJson AnnotatedWithBool)
instance HasJsonEncodingSpec AnnotatedWithBool where
  type EncodingSpec AnnotatedWithBool =
    'Module
      (JsonAnnotated
      '[ '("readOnly", 'True)
       , '("deprecated", 'False)
       ]
      (JsonObject '[ Required "name" JsonString ]))
instance TupleEncoding AnnotatedWithBool where
  toJsonStructure AnnotatedWithBool { awbName } =
    (Field @"name" awbName,
    ())
instance HasJsonDecodingSpec AnnotatedWithBool where
  type DecodingSpec AnnotatedWithBool = EncodingSpec AnnotatedWithBool
instance TupleDecoding AnnotatedWithBool where
  fromJsonStructure
      (Field @"name" awbName,
      ())
    =
      pure AnnotatedWithBool { awbName }

{- ========================================================================== -}

