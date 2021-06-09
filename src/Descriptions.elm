module Descriptions exposing (..)
import Html exposing (..)

sgOverall : Html a
sgOverall =
    span
        []
        [ text "Semantic gravity is about the relationship between "
        , em [] [ text "context" ]
        , text " and "
        , em [] [ text "meaning" ]
        , text ".  Knowledge with a lot of semantic gravity is embedded strongly within a particular concrete and delimited context.  Knowledge with little semantic gravity is generalized and abstract, applicable to many contexts."
        ]

sgPlus : Html a
sgPlus =
    span
        []
        [ text "Increased abstraction and generalization; less embedded in a specific and concrete context." ]

sgMinus : Html a
sgMinus =
    span
        []
        [ text "Increased binding to a particular context or situation; decreased applicability to many different contexts." ]