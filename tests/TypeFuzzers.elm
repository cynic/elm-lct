module TypeFuzzers exposing (..)
import Fuzz exposing (Fuzzer, int, list, string, float, bool)
import GenericDict as Dict exposing (Dict)
import SerializableData exposing (..)

configurationFuzzer : Fuzzer Configuration
configurationFuzzer =
    Fuzz.map
        (\es -> { eventSpacing = es })
        int

dimensionName : Fuzzer DimensionName
dimensionName =
    let
        whatever blah =
            case blah of
                SG ->
                    ()
                SD ->
                    ()
                IR ->
                    ()
                SubR ->
                    ()
                OR ->
                    ()
                DR ->
                    ()
                PA ->
                    ()
                RA ->
                    ()
    in
        Fuzz.oneOf
            [ Fuzz.constant SG
            , Fuzz.constant SD
            , Fuzz.constant IR
            , Fuzz.constant SubR
            , Fuzz.constant OR
            , Fuzz.constant DR
            , Fuzz.constant PA
            , Fuzz.constant RA
            ]

rangeDescription : Fuzzer RangeDescription
rangeDescription =
    Fuzz.map2
        (\d r ->
            { description = d
            , range = r
            }
        )
        string
        ( Fuzz.tuple ( float, float ) )

point : Fuzzer Point
point =
    let
        whatever blah =
            case blah of
                Value _ _ ->
                    ()
                Described _ _ _ ->
                    ()
    in
        Fuzz.oneOf
            [ Fuzz.map2 Value int float
            , Fuzz.map3 Described int float string
            ]

dimensionUX : Fuzzer DimensionUX
dimensionUX =
    Fuzz.oneOf
    [ Fuzz.constant Shown
    , Fuzz.constant Defocused
    , Fuzz.constant Focused
    ]

dimension : Fuzzer Dimension
dimension =
    Fuzz.map4
        (\texts points (plus, minus, color) ux ->
            { texts = texts
            , points = points
            , plus = plus
            , minus = minus
            , color = color
            , ux = ux
            }
        )
        ( Fuzz.list rangeDescription )
        ( Fuzz.list point )
        ( Fuzz.tuple3 ( string, string, string ) )
        ( dimensionUX )

dimensionsKeyValue : Fuzzer (DimensionName, Dimension)
dimensionsKeyValue =
    Fuzz.tuple ( dimensionName, dimension )

diagramFuzzer : Fuzzer Diagram
diagramFuzzer =
    Fuzz.map5
        (\(textHeight, width, graphHeight) events dimensions additionalInfo config ->
            { textHeight = textHeight
            , width = width
            , graphHeight = graphHeight
            , events = events
            , dimensions = Dict.fromList dimensionNameToString dimensions
            , config = config
            , additionalInfo = additionalInfo
            , ux =
                { interactable = Nothing
                }
            }
        )
        ( Fuzz.tuple3 ( int, int, int ) )
        ( Fuzz.list string )
        ( Fuzz.list dimensionsKeyValue )
        ( Fuzz.map2 AdditionalInfo bool bool )
        ( configurationFuzzer )
