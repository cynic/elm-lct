module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import SerializableData exposing (..)
import Serialize.Encode as E
import Serialize.Decode as D
import Json.Decode
import TypeFuzzers exposing (..)

serialization : Test
serialization =
    describe "The Serialization modules"
        [ fuzz diagramFuzzer "can round-trip encode and decode correctly" <|
            \value ->
                E.encodeDiagram value
                |> Json.Decode.decodeValue D.diagram
                |> Expect.equal (Ok value)
        ]

dimensionNames : Test
dimensionNames =
    describe "Dimension names"
        [ fuzz dimensionName "can round-trip from strings correctly" <|
            \value ->
                dimensionNameToString value
                |> stringToDimensionName
                |> Expect.equal (Just value)
        ]

{-
suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse" -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                        Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]
-}
{-
suite : Test
suite =
    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
-}