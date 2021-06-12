module Main exposing (..)
import Html exposing (div, Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String exposing (fromInt, fromFloat)
import List exposing (length)
import String exposing (toInt)
import Html.Events exposing (onClick)
import Browser exposing (element)
import Html exposing (button)
import Browser.Events exposing (onMouseMove)
import Json.Decode as D
import Svg.Events exposing (on)
-- height and width of these are 496.625
import FontAwesome.Solid
import FontAwesome.Svg exposing (viewIcon)
import FontAwesome.Styles exposing (css)
import Html.Attributes exposing (href)

iconSize : Float
iconSize = 512.0

-- diagram

valueChange : Float -- how much a position can change by, normally
valueChange =
    0.1

type alias EventLine =
    Int

type alias RangeDescription =
    { description : String
    , range : ( Float, Float )
    }

type Point
    = Value EventLine Float
    | Described EventLine Float String

type alias Dimension =
    { texts : List RangeDescription
    , points : List Point
    }

type Dimensions
    = SG
type alias Band = Int -- which band (0-3) we're drawing
type alias Configuration =
    { eventSpacing : Int
    }
-- What do I want to do with a Point?
-- 1. move it up
-- 2. move it down
-- 3. create a new point with more +
-- 4. create a new point with more -
-- 5. create a new point at the same level
-- 6. trash the point
-- 7. modify/add a description
type Interactable
    = PointInteraction Point
type alias Diagram =
    { textHeight : Int
    , width : Int
    , graphHeight : Int
    , events : List (String)
    , sg : Dimension
    , config : Configuration
    , focusedDimension : Maybe Dimensions
    , interactable : Maybe Interactable
    }

-- Message
type Message
    = PlaceSG
    | FocusOnSG
    | ShowUIForPoint Point

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

dimensionData : Diagram -> Dimensions -> Dimension
dimensionData diagram dim =
    case dim of
        SG ->
            diagram.sg

defaultConfig : Configuration
defaultConfig =
    { eventSpacing = 90    
    }

sgInit : Dimension
sgInit =
    { texts =
        [ { description = "Tied to a concrete context or situation, or tied to a set of known contexts or situations"
          , range = ( -0.5, 0.0 )
          }
        , { description = "Strongly tied to specific context(s) or situation(s); requires effort to abstract"
          , range = ( -1.0, -0.5 )
          }
        , { description = "Abstracted and generalized, with application still visible"
          , range = ( 0.0, 0.5 )
          }
        , { description = "Theoretical, generalized, and abstracted; requires effort to tie to a context"
          , range = ( 0.5, 1.0 )
          }
        ]
    , points = [Value 0 0.0, Value 1 0.5, Value 2 0.3, Described 3 0.4 "I ain't so dense, amirite?  Hat.  Hat.", Value 4 0.8]
    }
init : Diagram
init =
    { textHeight = 200
    , width = 600
    , graphHeight = 320
    , events =
        [ "What problem(s) does version control address?"
        , "Older (centralized) version control"
        , "Lock-modify-unlock paradigm"
        , "Teamwork issues with lock-modify-unlock"
        ]
    , sg = sgInit
    , config = defaultConfig
    , focusedDimension = Nothing
    , interactable = Nothing
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

eventLineToGraphX : Diagram -> EventLine -> Float
eventLineToGraphX diagram n =
    dia_withGraphX diagram (\x -> x + toFloat n * toFloat diagram.config.eventSpacing)

-- event line for event at index /n/
eventLine : Diagram -> Int -> Svg a
eventLine diagram n =
    line
        [ x1 (fromFloat (eventLineToGraphX diagram n))
        , x2 (fromFloat (eventLineToGraphX diagram n))
        , y1 (fromFloat (dia_withGraphY diagram identity))
        , y2 (fromFloat (dia_withGraphHeight diagram identity))
        , stroke "#555"
        , strokeWidth "1"
        , strokeDasharray "4 1"
        ]
        []

-- event text for event at index /n/, with string /s/
eventText : Diagram -> EventLine -> String -> Svg a
eventText diagram n s =
    g
        [ transform ("rotate(-40, " ++ fromFloat (eventLineToGraphX diagram n) ++ ", " ++ fromFloat (dia_withGraphY diagram identity) ++ ")")
        ]
        [ line
            [ x1 (fromFloat (eventLineToGraphX diagram n))
            , y1 (fromFloat (dia_withGraphY diagram identity))
            , x2 (fromFloat (eventLineToGraphX diagram n + 300))
            , y2 (fromFloat (dia_withGraphY diagram identity))
            , stroke "#ccc"
            , strokeDasharray "1 1"
            ]
            []
        , text_
            [ x (fromFloat (eventLineToGraphX diagram n))
            , y (fromFloat (dia_withGraphY diagram identity))
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

drawEvents : Diagram -> List (Svg a)
drawEvents diagram =
    List.indexedMap (event diagram) diagram.events

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

createSGLine : Diagram -> Diagram
createSGLine diagram =
    { diagram | sg = { sgInit | points = [ Value 0 0.0 ] } }

focusOn : Dimensions -> Diagram -> Diagram
focusOn dim diagram =
    { diagram | focusedDimension = Just dim }

showPointUI : Point -> Diagram -> Diagram
showPointUI point diagram =
    { diagram | interactable = Just (PointInteraction point) }

update : Message -> Diagram -> (Diagram, Cmd Message)
update message diagram =
    case message of
    FocusOnSG ->
        ( focusOn SG diagram , Cmd.none )
    PlaceSG ->
        ( createSGLine diagram, Cmd.none )
    ShowUIForPoint p ->
        ( showPointUI p diagram, Cmd.none )

pointToQuantitativeValue : Point -> Float
pointToQuantitativeValue point =
    case point of
        Value _ v ->
            v
        Described _ v _ ->
            v

pointToGraphY : Diagram -> Point -> Float
pointToGraphY diagram point =
    let
        v =
            pointToQuantitativeValue point
        inner h =
            if v == 0.0 then
                h / 2.0
            else if v > 0.0 then
                (h / 2.0) - (v * (h / 2.0))
            else
                (h / 2.0) + (-v * (h / 2.0))
    in
        dia_withGraphHeight diagram inner

pointToEventLine : Point -> EventLine
pointToEventLine point =
    case point of
        Value n _ ->
            n
        Described n _ _ ->
            n

pointToGraphCoordinates : Diagram -> Point -> ( Float, Float )
pointToGraphCoordinates diagram point =
    ( eventLineToGraphX diagram (pointToEventLine point), pointToGraphY diagram point )

drawPoint : Diagram -> Point -> Svg a
drawPoint diagram point =
    case point of
        Value n v ->
            circle
                [ cx (fromFloat (eventLineToGraphX diagram n))
                , cy (fromFloat (pointToGraphY diagram point))
                , r "5"
                , fill "#fcab30cc"
                , stroke "black"
                ]
                []
        Described n v description ->
            rect
                [ x (fromFloat (eventLineToGraphX diagram n - 5))
                , y (fromFloat (pointToGraphY diagram point - 5))
                , width "10"
                , height "10"
                , fill "#fcab30cc"
                , stroke "black"
                ]
                [ Svg.title [] [ text description ] ]

drawContinuousLine : Diagram -> List ( Float, Float ) -> Svg a
drawContinuousLine diagram coordinates =
    case coordinates of
        [] ->
            g [] []
        [_] ->
            g [] []
        (ix, iy)::rest ->
            Svg.path
                [ d
                    ( List.foldl (\(x, y) state ->
                        state ++ "C "
                        ++ fromFloat (x - 15) ++ " " ++ fromFloat y ++ ", "
                        ++ fromFloat (x + 15) ++ " " ++ fromFloat y ++ ", "
                        ++ fromFloat x ++ " " ++ fromFloat y ++ " "
                    ) ("M " ++ fromFloat ix ++ " " ++ fromFloat iy ++ " ") rest
                    )
                , stroke "#fcab30"
                , strokeWidth "2"
                , fill "transparent"
                ]
                []

drawLine : Diagram -> List Point -> Svg a
drawLine diagram points =
    List.map ( pointToGraphCoordinates diagram ) points
    |> drawContinuousLine diagram

drawPoints : Diagram -> List Point -> Svg a
drawPoints diagram points =
    g [] ( List.map (drawPoint diagram) points )

first : (a -> Bool) -> List a -> Maybe a
first predicate list =
    case list of
        [] ->
            Nothing
        h::rest ->
            if predicate h then
                Just h
            else
                first predicate rest

pointWithinRadius : Diagram -> Int -> (Int, Int) -> List Point -> Maybe Point
pointWithinRadius diagram radius (x, y) points =
    first
        (\point ->
            let
                (px, py) = pointToGraphCoordinates diagram point
                dx = px - toFloat x
                dy = py - toFloat y
            in
                sqrt (dx * dx + dy * dy) <= toFloat radius                
        )
        points

withinPointRadius : Diagram -> D.Decoder Message
withinPointRadius diagram =
    case diagram.focusedDimension |> Maybe.map (dimensionData diagram) of
        Nothing ->
            D.fail "No focused dimension"
        Just dimension ->
            D.map2 (\x y -> (x, y))
                (D.field "clientX" D.int)
                (D.field "clientY" D.int)
            |> D.andThen
                (\coords ->
                    case pointWithinRadius diagram 35 coords dimension.points of
                        Just point ->
                            D.succeed (ShowUIForPoint point)
                        Nothing ->
                            D.fail "No point within radius"
                )


drawPointInteractionUI : Diagram -> Point -> Svg a
drawPointInteractionUI diagram point =
    pointToGraphCoordinates diagram point
    |> (\(x, y) ->
        g
            []
            [ circle
                [ cx (fromFloat x)
                , cy (fromFloat y)
                , fill "#fff5"
                , stroke "black"
                , r "35"
                ]
                []
            , g
                [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") translate (16 -9) scale (0.03)")
                , Svg.Attributes.cursor "pointer"
                ]
                [ circle
                    [ r "256"
                    , fill "yellow"
                    , cx "256"
                    , cy "256"
                    , id "right"
                    ]
                    []
                , viewIcon FontAwesome.Solid.arrowCircleRight
                ]
            , g
                [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") rotate (-45) translate (16 -9) scale (0.03)")
                , Svg.Attributes.cursor "pointer"
                ]
                [ circle
                    [ r "256"
                    , fill "yellow"
                    , cx "256"
                    , cy "256"
                    ]
                    []
                , viewIcon FontAwesome.Solid.arrowCircleRight
                ] -- top-right
            , g
                [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") rotate (45) translate (16 -9) scale (0.03)")
                , Svg.Attributes.cursor "pointer"
                ]
                [ circle
                    [ r "256"
                    , fill "yellow"
                    , cx "256"
                    , cy "256"
                    ]
                    []
                , viewIcon FontAwesome.Solid.arrowCircleRight
                ] -- bottom-right
            , g
                [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") rotate (-90) translate (16 -9) scale (0.03)")
                , Svg.Attributes.cursor "pointer"
                ]
                [ circle
                    [ r "256"
                    , fill "yellow"
                    , cx "256"
                    , cy "256"
                    ]
                    []
                , viewIcon FontAwesome.Solid.arrowCircleRight
                ] -- top
            , g
                [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") rotate (90) translate (16 -9) scale (0.03)")
                , Svg.Attributes.cursor "pointer"
                ]
                [ circle
                    [ r "256"
                    , fill "yellow"
                    , cx "256"
                    , cy "256"
                    ]
                    []
                , viewIcon FontAwesome.Solid.arrowCircleRight
                ] -- bottom
            , g
                [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") translate (-26 -16) scale (0.03)")
                , Svg.Attributes.cursor "pointer"
                ]
                [ rect
                    [ Svg.Attributes.x "0"
                    , Svg.Attributes.y "50"
                    , width "462"
                    , height "462"
                    , fill "yellow"
                    ]
                    []
                , viewIcon FontAwesome.Solid.edit
                ] -- top-left
            , g
                [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") translate (-26 6) scale (0.03)")
                , color "red"
                , Svg.Attributes.cursor "pointer"
                ]
                [ viewIcon FontAwesome.Solid.trash
                ] -- top-left
            ]
    )

drawInteractable : Diagram -> Maybe Interactable -> Svg a
drawInteractable diagram interactable =
    case interactable of
        Nothing ->
            g [] []
        Just (PointInteraction point) ->
            drawPointInteractionUI diagram point
    
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
        (drawEvents diagram)
    , drawLine diagram diagram.sg.points
    , drawInteractable diagram diagram.interactable
    , drawPoints diagram diagram.sg.points
    ]

view : Diagram -> Html Message
view diagram =
    div
        []
        [ div
            [ Svg.Events.on "mousemove" (withinPointRadius diagram) ]
            [ svgView diagram ]
        , div
            []
            [ case diagram.sg.points of
              [] ->
                button
                    [ onClick PlaceSG ]
                    [ text "Create SG" ]
              _ ->
                button
                    [ onClick FocusOnSG ]
                    [ text "Focus on SG" ]
            ]
        ]

main : Program () Diagram Message
main =
    Browser.element
        { init = \_ -> (init, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }