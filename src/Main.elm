module Main exposing (..)
import Html exposing (div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String exposing (fromInt, fromFloat)
import List exposing (length)
import Browser
import String exposing (toInt)

-- diagram

type alias Band = Int -- which band (0-3) we're drawing
type alias Configuration =
    { eventSpacing : Int
    }
type alias Diagram =
    { textHeight : Int
    , width : Int
    , graphHeight : Int
    , events : List (String)
    , config : Configuration
    }

dia_withGraphWidth : Diagram -> (Float -> Float) -> Float
dia_withGraphWidth diagram f =
    f (toFloat diagram.width) + 20.0

dia_withGraphHeight : Diagram -> (Float -> Float) -> Float
dia_withGraphHeight diagram f =
    f (toFloat diagram.graphHeight) + toFloat diagram.textHeight

dia_withGraphX : Diagram -> (Float -> Float) -> Float
dia_withGraphX diagram f =
    f (toFloat 20)

dia_withGraphY : Diagram -> (Float -> Float) -> Float
dia_withGraphY diagram f =
    f (toFloat diagram.textHeight)

dia_fullHeight : Diagram -> Int
dia_fullHeight diagram =
    diagram.textHeight + diagram.graphHeight

dia_fullWidth : Diagram -> Int
dia_fullWidth diagram =
    diagram.width + 20

defaultConfig : Configuration
defaultConfig =
    { eventSpacing = 60
    }
init : Diagram
init =
    { textHeight = 200
    , width = 400
    , graphHeight = 220
    , events =
        [ "What problem(s) does version control address?"
        , "Older (centralized) version control"
        , "Lock-modify-unlock paradigm"
        , "Teamwork issues with lock-modify-unlock"
        ]
    , config = defaultConfig
    }

horizAxis : Diagram -> Svg a
horizAxis diagram =
    line
        [ x1 (fromFloat (dia_withGraphX diagram identity))
        , y1 (fromFloat (dia_withGraphHeight diagram (\h -> h/2)))
        , x2 (fromFloat (dia_withGraphWidth diagram identity))
        , y2 (fromFloat (dia_withGraphHeight diagram (\h -> h/2)))
        , stroke "black"
        , strokeWidth "2"
        ]
        []

vertAxis : Diagram -> Svg a
vertAxis diagram =
    g
        []
        [ text_
            [ x (fromFloat (dia_withGraphX diagram (\x -> x - 15)))
            , y (fromFloat (dia_withGraphY diagram (\y -> y + 13)))
            , fill "black"
            , fontFamily "Calibri"
            , fontSize "14pt"
            , fontWeight "bold"
            ]
            [ text "+"
            ]
        , circle
            [ cx (fromFloat (dia_withGraphX diagram (\x -> x - 10.5)))
            , cy (fromFloat (dia_withGraphY diagram (\y -> y + 8.5)))
            , r "8"
            , fill "transparent"
            , stroke "black"
            , strokeWidth "2"
            ]
            []
        , text_
            [ x (fromFloat (dia_withGraphX diagram (\x -> x - 15)))
            , y (fromFloat (dia_withGraphHeight diagram (\h -> h - 5)))
            , fill "black"
            , fontFamily "Calibri"
            , fontSize "14pt"
            , fontWeight "bold"
            ]
            [ text "–"
            ]
        , circle
            [ cx (fromFloat (dia_withGraphX diagram (\x -> x - 10.5)))
            , cy (fromFloat (dia_withGraphHeight diagram (\h -> h - 10)))
            , r "8"
            , fill "transparent"
            , stroke "black"
            , strokeWidth "2"
            ]
            []
        ]

-- event line for event at index /n/
eventLine : Diagram -> Int -> Svg a
eventLine diagram n =
    line
        [ x1 (fromFloat (dia_withGraphX diagram (\x -> x + toFloat n * toFloat diagram.config.eventSpacing)))
        , x2 (fromFloat (dia_withGraphX diagram (\x -> x + toFloat n * toFloat diagram.config.eventSpacing)))
        , y1 (fromFloat (dia_withGraphY diagram identity))
        , y2 (fromFloat (dia_withGraphHeight diagram identity))
        , stroke "#555"
        , strokeWidth "1"
        , strokeDasharray "4 1"
        ]
        []

-- event text for event at index /n/, with string /s/
eventText : Diagram -> Int -> String -> Svg a
eventText diagram n s =
    let
        x_ = dia_withGraphX diagram (\x -> x + toFloat n * toFloat diagram.config.eventSpacing)
        y_ = dia_withGraphY diagram identity
        x__ = fromFloat x_
        y__ = fromFloat y_
    in
        g
            [ transform ("rotate(-40, " ++ x__ ++ ", " ++ y__ ++ ")")
            ]
            [ line
                [ x1 x__
                , y1 y__
                , x2 (fromFloat (x_ + 300))
                , y2 y__
                , stroke "#ccc"
                , strokeDasharray "1 1"
                ]
                []
            , text_
                [ x x__
                , y y__
                , fill "black"
                --, textLength (fromInt diagram.config.eventSpacing ++ "px")
                , fontFamily "Calibri, sans-serif"
                , fontSize "11pt"
                ]
                [ text s
                ]
            ]

event : Diagram -> Int -> String -> Svg a
event diagram n s =
    g
        []
        [ eventLine diagram n
        , eventText diagram n s
        ]

events : Diagram -> List (Svg a)
events diagram =
    List.indexedMap (event diagram) diagram.events

-- band : this is the "band" from 
band : Diagram -> Band -> Svg a
band diagram n =
    let
        color =
            if n == 0 || n == 3 then
                "#00b058"
            else
                "#00ff82"
    in
        rect
            [ x (fromFloat (dia_withGraphX diagram identity))
            , width (fromFloat (dia_withGraphWidth diagram identity))
            , y (fromFloat (dia_withGraphHeight diagram (\h -> toFloat n / 4 * h)))
            , height (fromFloat (dia_withGraphHeight diagram (\h -> (toFloat n+1)+1/4 * h)))
            , fill color
            ]
            []

svgView : Diagram -> Svg a
svgView diagram =
  svg
    [ width (fromInt (dia_fullWidth diagram))
    , height (fromInt (dia_fullHeight diagram))
    , viewBox ("0 0 " ++ fromInt (dia_fullWidth diagram) ++ " " ++ fromInt (dia_fullHeight diagram))
    ]
    [ band diagram 0
    , band diagram 1
    , band diagram 2
    , band diagram 3
    , horizAxis diagram
    , vertAxis diagram
    , g
        []
        (events diagram)
    ]

update : msg -> Diagram -> (Diagram, Cmd msg)
update _ diagram =
    (diagram, Cmd.none)

view diagram =
    div [] [svgView diagram]

main : Program () Diagram msg
main =
    Browser.element
        { init = \_ -> (init, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }