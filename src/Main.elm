module Main exposing (..)
import Html exposing (div, Html, button)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String exposing (fromInt, fromFloat)
import List
import Browser
import Json.Decode as D
import Svg.Events exposing (on)
import FontAwesome.Solid
import FontAwesome.Svg exposing (viewIcon)
import Svg.Events exposing (onClick)
import GenericDict as Dict exposing (Dict)

-- TODO: Should be able to edit event text
-- TODO: More than one dimension...
-- TODO: Focus-buttons on SVG itself, no HTML necessary??
-- TODO: Descriptions for nodes
-- TODO: Better bands, which actually reflect the ranges and meanings of the focused dimension
-- TODO: Better UX for moving a point up and down ... perhaps dragging and/or keypresses?
-- TODO: Band colours should be reflective of focused dimension.
-- TODO: (Limited?) Undo?

iconSize : Float
iconSize = 512.0

-- diagram

valueChange : Float -- how much a position can change by, normally
valueChange =
    0.1

type alias EventLine =
    Int

type alias Event =
    String

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
    , plus : String
    , minus : String
    }

type DimensionName
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
type PointInteraction
    = ShowPointUI
    | MovePointUp
    | MovePointDown
    | ExtendPointUp
    | ExtendPointDown
    | ExtendPoint
    | DeletePoint

type EventInteraction
    = DeleteEvent
    | InsertEventAfter

type Interactable
    = InteractablePoint Point
type alias Diagram =
    { textHeight : Int
    , width : Int
    , graphHeight : Int
    , events : List Event
    , dimensions : Dict DimensionName Dimension
    , config : Configuration
    , focusedDimension : Maybe DimensionName
    , interactable : Maybe Interactable
    }

-- Message
type Message
    = FocusOn DimensionName
    | DoWithPoint PointInteraction Point
    | RemovePointUI
    | DoWithEvent EventInteraction EventLine

dia_withGraphWidth : Diagram -> (Float -> Float) -> Float
dia_withGraphWidth diagram f =
    f (toFloat diagram.width) + 30.0

dia_withGraphHeight : Diagram -> (Float -> Float) -> Float
dia_withGraphHeight diagram f =
    f (toFloat diagram.graphHeight) + toFloat diagram.textHeight

dia_withGraphX : Diagram -> (Float -> Float) -> Float
dia_withGraphX diagram f =
    f (toFloat 30)

dia_withGraphY : Diagram -> (Float -> Float) -> Float
dia_withGraphY diagram f =
    f (toFloat diagram.textHeight)

dia_fullHeight : Diagram -> Int
dia_fullHeight diagram =
    diagram.textHeight + diagram.graphHeight + 35 -- the 35 is for under-graph space

dia_fullWidth : Diagram -> Int
dia_fullWidth diagram =
    diagram.width + 30

dimensionNameToString : DimensionName -> String
dimensionNameToString dim =
    case dim of
        SG ->
            "SG"

dimensionNameToDimension : Diagram -> DimensionName -> Maybe Dimension
dimensionNameToDimension diagram dim =
    case dim of
        SG ->
            dictGet dim diagram.dimensions

dictGet : DimensionName -> Dict DimensionName v -> Maybe v
dictGet =
    Dict.get dimensionNameToString

dictInsert : DimensionName -> v -> Dict DimensionName v -> Dict DimensionName v
dictInsert =
    Dict.insert dimensionNameToString

defaultConfig : Configuration
defaultConfig =
    { eventSpacing = 90    
    }

calculateWidth : Diagram -> Diagram
calculateWidth diagram =
    -- this is a bit of a thumb-suck and simplificaton, but with any luck, it SHOULD work OK.
    { diagram | width = diagram.config.eventSpacing * List.length diagram.events + 250 }

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
    , points =
        [ Value 0 0.0 ]
        --[Value 0 0.0, Value 1 0.5, Value 2 0.9, Described 3 1.0 "I ain't so dense, amirite?  Hat.  Hat.", Value 4 -0.9, Value 5 -1.0]
    , plus = "Increased abstraction and generalization; less embedded in a specific and concrete context"
    , minus = "Increased binding to a particular context or situation; decreased applicability to many different contexts"
    }
init : Diagram
init =
    { textHeight = 320
    , width = 0 -- will be calculated
    , graphHeight = 320
    , events =
        [ "What problem(s) does version control address?"
        , "Older (centralized) version control"
        , "Lock-modify-unlock paradigm"
        , "Sequence diagram for lock-modify-unlock - practical example"
        , "Teamwork issues with lock-modify-unlock"
        , "Alternative: copy-modify-merge"
        , "Sequence diagram for copy-modify-merge"
        ]
    , dimensions =
        Dict.fromList dimensionNameToString
            [ ( SG, sgInit )
            ]
    , config = defaultConfig
    , focusedDimension = Nothing
    , interactable = Nothing
    }
    |> calculateWidth

horizAxis : Diagram -> Svg a
horizAxis diagram =
    line
        [ x1 (fromFloat (dia_withGraphX diagram identity))
        , y1 (fromFloat (dia_withGraphHeight diagram (\h -> h/2)))
        , x2 (fromFloat (dia_withGraphWidth diagram identity))
        , y2 (fromFloat (dia_withGraphHeight diagram (\h -> h/2)))
        , stroke "black"
        , strokeWidth "3"
        ]
        []

vertAxis : Diagram -> Svg a
vertAxis diagram =
    g
        []
        [ g
            [ transform ("translate (" ++ fromFloat (dia_withGraphX diagram (\x -> x - 28)) ++ " " ++ fromFloat (dia_withGraphY diagram identity) ++ ") scale (0.05)")
            ]
            [ viewIcon FontAwesome.Solid.plusCircle ]
        , g
            [ transform ("translate (" ++ fromFloat (dia_withGraphX diagram (\x -> x - 28)) ++ " " ++ fromFloat (dia_withGraphHeight diagram (\h -> h - 28)) ++ ") scale (0.05)")
            ]
            [ viewIcon FontAwesome.Solid.minusCircle ]
        ]

eventLineToGraphX : Diagram -> EventLine -> Float
eventLineToGraphX diagram n =
    dia_withGraphX diagram (\x -> x + toFloat n * toFloat diagram.config.eventSpacing)

-- event line for event at index /n/
drawEventLine : Diagram -> EventLine -> Svg a
drawEventLine diagram n =
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
drawEventText : Diagram -> EventLine -> String -> Svg a
drawEventText diagram n s =
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
            , fontSize "14pt"
            ]
            [ text s
            ]
        ]

drawEventControls : Diagram -> EventLine -> Svg Message
drawEventControls diagram n =
    g
        []
        [ g
            [ transform ("translate (" ++ fromFloat (eventLineToGraphX diagram n + 5) ++ " " ++ fromFloat (dia_withGraphHeight diagram identity + 5) ++ ") scale (0.03)")
            , onClick (DoWithEvent InsertEventAfter n)
            ]
            [ rect
                [ x "60"
                , y "60"
                , width "332"
                , height "342"
                , fill "white"
                ]
                []
            , viewIcon FontAwesome.Solid.plusSquare
            , Svg.title
                []
                [ text "Insert new event" ]
            ]
        , if n > 0 then
            g
                [ transform ("translate (" ++ fromFloat (eventLineToGraphX diagram n - 20) ++ " " ++ fromFloat (dia_withGraphHeight diagram identity + 5) ++ ") scale (0.03)")
                , onClick (DoWithEvent DeleteEvent n)
                ]
                [ viewIcon FontAwesome.Solid.trash
                , Svg.title
                    []
                    [ text "Delete event" ]
                ]
          else
            g [] []
        ]

event : Diagram -> EventLine -> String -> Svg Message
event diagram n s =
    g
        []
        [ drawEventLine diagram n
        , drawEventText diagram n s
        , drawEventControls diagram n
        ]

drawEvents : Diagram -> Svg Message
drawEvents diagram =
    g
        []
        (List.indexedMap (event diagram) diagram.events)
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
            , width (fromFloat (toFloat diagram.width))
            , y (fromFloat (dia_withGraphHeight diagram (\h -> toFloat n / 4 * h)))
            , height (fromFloat (toFloat diagram.graphHeight / 4.0))
            , fill color
            ]
            []

focusOn : DimensionName -> Diagram -> Diagram
focusOn dim diagram =
    { diagram | focusedDimension = Just dim }

showPointUI : Point -> Diagram -> Diagram
showPointUI point diagram =
    { diagram | interactable = Just (InteractablePoint point) }

removePointUI : Diagram -> Diagram
removePointUI diagram =
    { diagram | interactable = Nothing }

changeFocusedDimension : Diagram -> (Dimension -> Dimension) -> Diagram
changeFocusedDimension diagram f =
    case diagram.focusedDimension of
        Just dim ->
            case dictGet dim diagram.dimensions of
                Nothing ->
                    diagram
                Just dimension ->
                    { diagram | dimensions = dictInsert dim (f dimension) diagram.dimensions }
        Nothing ->
            diagram

pointWithValue : (Float -> Float) -> Point -> Point
pointWithValue f point =
    case point of
        Value n v ->
            Value n (f v)
        Described n v s ->
            Described n (f v) s

pointWithEventLine : (EventLine -> EventLine) -> Point -> Point
pointWithEventLine f point =
    case point of
        Value n v ->
            Value (f n) v
        Described n v s ->
            Described (f n) v s

addPointToDimension : Dimension -> Point -> Dimension
addPointToDimension dimension point =
    { dimension | points = point :: dimension.points }

changePointInDimension : (Point -> Bool) -> (Point -> Point) -> Dimension -> Dimension
changePointInDimension predicate f dimension =
    { dimension | points =
        List.map (\p ->
            if predicate p then
                --pointWithValue (\v -> v + change) point
                f p
            else
                p
        ) dimension.points
    }

movePoint : Float -> Point -> Point
movePoint change point =
    pointWithValue (\v -> v + change) point

addExtendedPoint : Point -> Dimension -> Dimension
addExtendedPoint point dimension =
    pointWithEventLine (\eventLine -> eventLine + 1) point
    |> addPointToDimension dimension

movePointUp : Float -> Point -> Point
movePointUp change =
    movePoint change

movePointDown : Float -> Point -> Point
movePointDown change =
    movePoint -change

extendPointUp : Point -> Dimension -> Dimension
extendPointUp point =
    addExtendedPoint (movePointUp (valueChange * 2) point)

extendPointDown : Point -> Dimension -> Dimension
extendPointDown point =
    addExtendedPoint (movePointDown (valueChange * 2) point)

deletePoint : Point -> Dimension -> Dimension
deletePoint point dimension =
    { dimension | points = List.filter (\p -> p /= point) dimension.points }

interactivelyChangePointOnDiagram : Diagram -> Point -> (Point -> Point) -> Diagram
interactivelyChangePointOnDiagram diagram point f =
    changeFocusedDimension diagram (changePointInDimension ((==) point) f)
    |> showPointUI (f point)

interactivelyChangeDimensionOnDiagram : Diagram -> (Dimension -> Dimension) -> Diagram
interactivelyChangeDimensionOnDiagram diagram f =
    changeFocusedDimension diagram f

insertEventAfter : Diagram -> EventLine -> Diagram
insertEventAfter diagram n =
    let
        newEvents =
            List.concat
                [ List.take (n+1) diagram.events
                , ["New event"]
                , List.drop (n+1) diagram.events
                ]
        newDimensions =
            Dict.map (\_ dimension ->
                { dimension | points =
                    List.map
                        (\point ->
                            if pointToEventLine point > n then
                                pointWithEventLine (\eventLine -> eventLine + 1) point
                            else
                                point
                        )
                        dimension.points
                }
            ) diagram.dimensions
        
    in
    { diagram | events = newEvents, dimensions = newDimensions }
    |> calculateWidth

deleteEvent : Diagram -> EventLine -> Diagram
deleteEvent diagram n =
    let
        newEvents =
            List.concat
                [ List.take n diagram.events
                , List.drop (n+1) diagram.events
                ]
        newDimensions =
            -- Two functions:
            -- 1. Remove all points that are being used with this event.
            -- 2. Shift the event-references of all subsequent points back by 1.
            Dict.map (\_ dimension ->
                { dimension | points =
                    List.filterMap
                        (\point ->
                            if pointToEventLine point == n then
                                Nothing
                            else if pointToEventLine point > n then
                                Just <| pointWithEventLine (\eventLine -> eventLine - 1) point
                            else
                                Just point
                        )
                        dimension.points
                }
            ) diagram.dimensions
    in
        -- out of an abundance of caution (and laziness!), set the interactable as Nothing too.
        -- Perhaps it was displaying a Point that is now gone.
        { diagram | events = newEvents, dimensions = newDimensions, interactable = Nothing }
        |> calculateWidth

update : Message -> Diagram -> (Diagram, Cmd Message)
update message diagram =
    case message of
        FocusOn dimension ->
            ( focusOn dimension diagram , Cmd.none )
        DoWithPoint ShowPointUI p ->
            ( showPointUI p diagram, Cmd.none )
        DoWithPoint MovePointUp p ->
            ( interactivelyChangePointOnDiagram diagram p (movePointUp valueChange), Cmd.none )
        DoWithPoint MovePointDown p ->
            ( interactivelyChangePointOnDiagram diagram p (movePointDown valueChange), Cmd.none )
        DoWithPoint ExtendPoint p ->
            ( interactivelyChangeDimensionOnDiagram diagram (addExtendedPoint p), Cmd.none )
        DoWithPoint ExtendPointUp p ->
            ( interactivelyChangeDimensionOnDiagram diagram (extendPointUp p), Cmd.none )
        DoWithPoint ExtendPointDown p ->
            ( interactivelyChangeDimensionOnDiagram diagram (extendPointDown p), Cmd.none )
        DoWithPoint DeletePoint p ->
            ( interactivelyChangeDimensionOnDiagram diagram (deletePoint p), Cmd.none )
        RemovePointUI ->
            ( removePointUI diagram, Cmd.none )
        DoWithEvent InsertEventAfter n ->
            ( insertEventAfter diagram n, Cmd.none )
        DoWithEvent DeleteEvent n ->
            ( deleteEvent diagram n, Cmd.none )

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
        Value n _ ->
            circle
                [ cx (fromFloat (eventLineToGraphX diagram n))
                , cy (fromFloat (pointToGraphY diagram point))
                , r "5"
                , fill "#fcab30cc"
                , stroke "black"
                ]
                []
        Described n _ description ->
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
        (ix, iy)::_ ->
            Svg.path
                [ d
                    ( List.foldl (\(x, y) state ->
                        state
                        ++ fromFloat (x - (toFloat diagram.config.eventSpacing / 3)) ++ " " ++ fromFloat y ++ ", "
                        ++ fromFloat x ++ " " ++ fromFloat y ++ " "
                    ) ("M " ++ fromFloat ix ++ " " ++ fromFloat iy ++ " S ") coordinates
                    )
                , stroke "#fcab30"
                , strokeWidth "2"
                , fill "transparent"
                ]
                []

drawLine : Diagram -> List Point -> Svg a
drawLine diagram =
    List.sortBy ( pointToEventLine )
    >> List.map ( pointToGraphCoordinates diagram )
    >> ( drawContinuousLine diagram )

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
    case diagram.focusedDimension |> Maybe.andThen (dimensionNameToDimension diagram) of
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
                            D.succeed (DoWithPoint ShowPointUI point)
                        Nothing ->
                            case diagram.interactable of
                                Just (InteractablePoint _) ->
                                    -- I am outside the radius.
                                    -- No matter what the Point interaction is, it dies now.
                                    D.succeed RemovePointUI
                                Nothing ->
                                    D.fail "No point within radius"
                )

-- is this a point that can be extended to the next event line?
--   - In other words: is there already a point n+1 ??
-- is there an event line that this point can be extended to?
--   - In other words: is there an eventLine n+1 ??
-- if the answer to BOTH is "yes", then it is an "extension point"

canExtendFrom : Dimension -> Point -> Bool
canExtendFrom dimension point =
    let
        pEventLine =
            pointToEventLine point
    in
        -- I can only extend from a point if there isn't already another point that
        -- occupies the same space.
        List.all (\p -> pointToEventLine p /= pEventLine + 1) dimension.points

eventLineExists : Diagram -> EventLine -> Bool
eventLineExists diagram eventLine =
    eventLine < List.length diagram.events

isExtensionPoint : Diagram -> Dimension -> Point -> Bool
isExtensionPoint diagram dimension point =
    canExtendFrom dimension point && eventLineExists diagram (pointToEventLine point + 1)

drawPointInteractionUI : Diagram -> Dimension -> Point -> Svg Message
drawPointInteractionUI diagram dimension point =
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
                [] -- background translucent white circle
            , if isExtensionPoint diagram dimension point then
                g
                    [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") translate (16 -9) scale (0.03)")
                    , Svg.Attributes.cursor "pointer"
                    , onClick (DoWithPoint ExtendPoint point)
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
                    , Svg.title
                        []
                        [ text "Extend to next event, at the same level" ]
                    ] -- right-arrow
              else
                g [] []
            , if isExtensionPoint diagram dimension point && pointToQuantitativeValue point + valueChange <= 1.0 then
                g
                    [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") rotate (-45) translate (16 -9) scale (0.03)")
                    , Svg.Attributes.cursor "pointer"
                    , onClick (DoWithPoint ExtendPointUp point)
                    ]
                    [ circle
                        [ r "256"
                        , fill "yellow"
                        , cx "256"
                        , cy "256"
                        ]
                        []
                    , viewIcon FontAwesome.Solid.arrowCircleRight
                    , Svg.title
                        []
                        [ text ("Extend with: " ++ dimension.plus) ]
                    ] -- top-right
              else
                g [] []
            , if isExtensionPoint diagram dimension point && pointToQuantitativeValue point - valueChange >= -1.0 then
                g
                    [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") rotate (45) translate (16 -9) scale (0.03)")
                    , Svg.Attributes.cursor "pointer"
                    , onClick (DoWithPoint ExtendPointDown point)
                    ]
                    [ circle
                        [ r "256"
                        , fill "yellow"
                        , cx "256"
                        , cy "256"
                        ]
                        []
                    , viewIcon FontAwesome.Solid.arrowCircleRight
                    , Svg.title
                        []
                        [ text ("Extend with: " ++ dimension.minus) ]
                    ] -- bottom-right
              else
                g [] []
            , if pointToQuantitativeValue point + valueChange <= 1.0 then
                g
                    [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") rotate (-90) translate (16 -9) scale (0.03)")
                    , Svg.Attributes.cursor "pointer"
                    , onClick (DoWithPoint MovePointUp point)
                    ]
                    [ circle
                        [ r "256"
                        , fill "yellow"
                        , cx "256"
                        , cy "256"
                        ]
                        []
                    , viewIcon FontAwesome.Solid.arrowCircleRight
                    , Svg.title
                        []
                        [ text dimension.plus ]
                    ] -- top
              else
                g [] []
            , if pointToQuantitativeValue point - valueChange >= -1.0 then
                g
                    [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") rotate (90) translate (16 -9) scale (0.03)")
                    , Svg.Attributes.cursor "pointer"
                    , onClick (DoWithPoint MovePointDown point)
                    ]
                    [ circle
                        [ r "256"
                        , fill "yellow"
                        , cx "256"
                        , cy "256"
                        ]
                        []
                    , viewIcon FontAwesome.Solid.arrowCircleRight
                    , Svg.title
                        []
                        [ text dimension.minus ]
                    ] -- bottom
              else
                g [] []
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
                , Svg.title
                    []
                    [ text "Justify/Explain point at this level" ]
                ] -- edit icon
            , if pointToEventLine point > 0 then
                g
                    [ transform ("translate (" ++ fromFloat x ++ " " ++ fromFloat y ++ ") translate (-26 6) scale (0.03)")
                    , color "red"
                    , Svg.Attributes.cursor "pointer"
                    , onClick (DoWithPoint DeletePoint point)
                    ]
                    [ viewIcon FontAwesome.Solid.trash
                    , Svg.title
                        []
                        [ text "Delete this point" ]
                    ] -- trash icon
              else
                g [] []
            ]
    )

drawInteractable : Diagram -> Maybe Interactable -> Svg Message
drawInteractable diagram interactable =
    case ( interactable, diagram.focusedDimension |> Maybe.andThen (dimensionNameToDimension diagram) ) of
        ( Nothing, _ ) ->
            g [] []
        ( _, Nothing ) ->
            g [] []
        ( Just (InteractablePoint point), Just dimension ) ->
            drawPointInteractionUI diagram dimension point

sortByFocused : Diagram -> List Dimension -> List Dimension
sortByFocused diagram dimensions =
    case diagram.focusedDimension |> Maybe.andThen (dimensionNameToDimension diagram) of
        Nothing ->
            dimensions
        Just focused ->
            List.sortBy (\dimension -> if dimension == focused then 1 else 0) dimensions

drawDimensionsLines : Diagram -> Svg a
drawDimensionsLines diagram =
    g
        []
        ( Dict.values diagram.dimensions
          |> sortByFocused diagram
          |> List.map (\dimension -> dimension.points)
          |> List.map (drawLine diagram)
        )

drawDimensionsPoints : Diagram -> Svg a
drawDimensionsPoints diagram =
    g
        []
        ( Dict.values diagram.dimensions
          |> sortByFocused diagram
          |> List.map (\dimension -> dimension.points)
          |> List.map (drawPoints diagram)
        )

svgView : Diagram -> Svg Message
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
    , drawEvents diagram
    , drawDimensionsLines diagram
    , drawInteractable diagram diagram.interactable
    , drawDimensionsPoints diagram
    ]

view : Diagram -> Html Message
view diagram =
    div
        []
        [ div
            [ Svg.Events.on "mousemove" (withinPointRadius diagram)
            ]
            [ svgView diagram ]
        , div
            []
            [ button
                [ onClick (FocusOn SG) ]
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