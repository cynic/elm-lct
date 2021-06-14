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
