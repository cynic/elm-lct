module Serialize.Encode exposing (..)
import SerializableData exposing (..)
import Json.Encode as E
import GenericDict as Dict

-- Current Version

currentVersion : Int
currentVersion = 0

event : Event -> E.Value
event e =
    E.string e

range : ( Float, Float ) -> E.Value
range (a,b) =
    E.object
        [ ( "a", E.float a )
        , ( "b", E.float b )
        ]

rangeDescription : RangeDescription -> E.Value
rangeDescription rdesc =
    E.object
        [ ( "description", E.string rdesc.description )
        , ( "range", range rdesc.range )
        ]

point : Point -> E.Value
point p =
    case p of
        Value e f ->
            E.object
                [ ( "type", E.string "value" )
                , ( "eventline", E.int e )
                , ( "value", E.float f )
                ]
        Described e f s ->
            E.object
                [ ( "type", E.string "described" )
                , ( "eventline", E.int e )
                , ( "value", E.float f )
                , ( "desc", E.string s )
                ]

dimension : Dimension -> E.Value
dimension dim =
    E.object
        [ ( "texts", E.list rangeDescription dim.texts )
        , ( "points", E.list point dim.points )
        , ( "plus", E.string dim.plus )
        , ( "minus", E.string dim.minus )
        , ( "color", E.string dim.color )
        ]

dimensionKeyValue : ( DimensionName, Dimension ) -> E.Value
dimensionKeyValue (name, dim) =
    E.object
        [ ( "key", E.string (dimensionNameToString name) )
        , ( "value", dimension dim )
        ]

config : Configuration -> E.Value
config configuration =
    E.object
        [ ( "eventSpacing", E.int configuration.eventSpacing )
        ]

encodeDiagram : Diagram -> E.Value
encodeDiagram diagram =
    E.object
        [ ( "version", E.int currentVersion )
        , ( "textHeight", E.int diagram.textHeight )
        , ( "width", E.int diagram.width )
        , ( "graphHeight", E.int diagram.graphHeight )
        , ( "events", E.list event diagram.events )
        , ( "dimensions", E.list dimensionKeyValue (Dict.toList diagram.dimensions) )
        , ( "config", config diagram.config )
        ]