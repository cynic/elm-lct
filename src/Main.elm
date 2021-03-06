{-
    Elm-LCT, software tooling for LCT practitioners.
    Copyright (C) 2022 Yusuf M Motara

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
module Main exposing (..)
import Html exposing (Html)
import ListExtensions exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String exposing (fromInt, fromFloat)
import List
import Dict as ElmDict
import Browser
import Json.Decode as D
import FontAwesome.Solid
import FontAwesome.Svg exposing (viewIcon)
import Svg.Events exposing (onClick)
import GenericDict as Dict exposing (Dict)
import Browser.Events
import Html.Attributes
import SerializableData exposing (..)
import Dimensions
import Serialize.Decode
import Serialize.Encode
import Json.Encode
import File.Download as Download
import File.Select as Select
import File exposing (File)
import Json.Decode
import Task

valueChange : Float -- how much a position can change by, normally
valueChange =
    0.1

type FileLoadProcess
    = RequestLoad
    | Select File
    | Loaded String -- file content

type InferredLine
    = Gaze
    | Epistemic

-- Message
type Message
    = FocusOn DimensionName
    | DefocusOn DimensionName
    | OpenMenu
    | CloseMenu
    | ShowDimension DimensionName
    | DoWithPoint PointInteraction Point
    | ShowInferred InferredLine
    | HideInferred InferredLine
    | RemovePointUI
    | DoWithEvent EventInteraction EventLine
    | EditTargetContext
    | UpdateText TextAction
    | Save Diagram
    | Load FileLoadProcess

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

dimensionNameToDimension : Diagram -> DimensionName -> Maybe Dimension
dimensionNameToDimension diagram dim =
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
    { diagram | width = diagram.config.eventSpacing * List.length diagram.events + 260 }

init : Diagram
init =
    { textHeight = 320
    , width = 0 -- will be calculated
    , graphHeight = 400
    , events =
        ["New event"]
{-
        [ "What problem(s) does version control address?"
        , "Older (centralized) version control"
        , "Lock-modify-unlock paradigm"
        , "Sequence diagram for lock-modify-unlock - practical example"
        , "Teamwork issues with lock-modify-unlock"
        , "Alternative: copy-modify-merge"
        , "Sequence diagram for copy-modify-merge"
        ]
-}
    , dimensions =
        Dict.fromList dimensionNameToString
            [ ( SG, Dimensions.sgInit )
            , ( SD, Dimensions.sdInit )
            , ( IR, Dimensions.irInit )
            , ( SubR, Dimensions.subrInit )
            , ( OR, Dimensions.orInit )
            , ( DR, Dimensions.drInit )
            , ( PA, Dimensions.paInit )
            , ( RA, Dimensions.raInit )
            ]
    , targetContext = "Target context"
    , config = defaultConfig
    , additionalInfo =
        { showGaze = True
        , showEpistemic = True
        }
    , ux =
        { interactable = Nothing
        , menuShown = False
        }
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
drawEventText : Diagram -> EventLine -> String -> Svg Message
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
            [ x (fromFloat (eventLineToGraphX diagram n + 8))
            , y (fromFloat (dia_withGraphY diagram identity))
            , fill "black"
            , fontFamily "Calibri, sans-serif"
            , fontSize "14pt"
            ]
            [ text s
            ]
        , g
            [ transform ("translate (" ++ fromFloat (eventLineToGraphX diagram n + 50) ++ " " ++ fromFloat (dia_withGraphY diagram identity + 5) ++ ") scale (0.03)")
            , onClick (DoWithEvent (EditEventText { maxLength = 80, current = s, cursor = End }) n)
            ]
            [ rect
                    [ Svg.Attributes.x "0"
                    , Svg.Attributes.y "50"
                    , width "462"
                    , height "462"
                    , fill "white"
                    ]
                    []
            , viewIcon FontAwesome.Solid.edit
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
                "#C0C0C0"
            else
                "#DCDCDC"
    in
        rect
            [ x (fromFloat (dia_withGraphX diagram identity))
            , width (fromFloat (toFloat diagram.width))
            , y (fromFloat (dia_withGraphHeight diagram (\h -> toFloat n / 4 * h)))
            , height (fromFloat (toFloat diagram.graphHeight / 4.0))
            , fill color
            ]
            []

modifyUX : Diagram -> (UX -> UX) -> Diagram
modifyUX diagram f =
    { diagram | ux = f diagram.ux }

showTextUI : Diagram -> TextData -> PostTextEdit -> Diagram
showTextUI diagram textData postTextEdit =
    modifyUX diagram (\ux ->
        { ux | interactable = Just (InteractableText textData postTextEdit) }
    )

showPointUI : Point -> Diagram -> Diagram
showPointUI point diagram =
    modifyUX diagram (\ux ->
        { ux | interactable = Just (InteractablePoint point) }
    )

removeInteractableUI : Diagram -> Diagram
removeInteractableUI diagram =
    modifyUX diagram (\ux ->
        { ux | interactable = Nothing }
    )

changeDimension : DimensionName -> Diagram -> (Dimension -> Dimension) -> Diagram
changeDimension dimensionName diagram f =
    case dictGet dimensionName diagram.dimensions of
        Nothing ->
            diagram
        Just dimension ->
            { diagram | dimensions = dictInsert dimensionName (f dimension) diagram.dimensions }

withFocusedDimension : any -> Dict DimensionName Dimension -> (any -> DimensionName -> Dimension -> any) -> any
withFocusedDimension foldable dimensions f =
    Dict.fold
        (\k dim state ->
            case dim.ux of
                Focused ->
                    f state k dim
                _ ->
                    state
        )
        foldable
        dimensions

changeFocusedDimension : Diagram -> (Dimension -> Dimension) -> Diagram
changeFocusedDimension diagram f =
    Dict.fold
        (\k dim state ->
            case dim.ux of
                Focused ->
                    changeDimension k state f
                _ ->
                    state
        )
        diagram
        diagram.dimensions

-- only one can be focused at a time, so we need some extra logic here.
focusOn : DimensionName -> Diagram -> Diagram
focusOn chosenDimension diagram =
    Dict.fold
        (\k dim state ->
            if chosenDimension == k then
                changeDimension k state
                    (\d ->
                        { d | ux = Focused }
                    )
            else
                case dim.ux of
                    Focused ->
                        changeDimension k state
                            (\d ->
                                { d | ux = Shown }
                            )
                    _ ->
                        state
        )
        diagram
        diagram.dimensions

defocusOn : DimensionName -> Diagram -> Diagram
defocusOn dim diagram =
    changeDimension dim diagram (\dimension ->
        { dimension | ux = Defocused }
    )

reshow : DimensionName -> Diagram -> Diagram
reshow dim diagram =
    changeDimension dim diagram (\dimension ->
        { dimension | ux = Shown }
    )

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

newPointAfter : Point -> Point
newPointAfter point =
    case point of
        Value n v ->
            Value (n + 1) v
        Described n v _ ->
            Value (n + 1) v

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
    newPointAfter point
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
        { diagram | events = newEvents, dimensions = newDimensions }
        |> removeInteractableUI
        |> calculateWidth

pointWithText : String -> Point -> Point
pointWithText s point =
    if String.length s > 0 then
        case point of
            Described n v _ ->
                Described n v s
            Value n v ->
                Described n v s
    else
        case point of
            Described n v _ ->
                Value n v
            Value _ _ ->
                point

handleTextDecision : Diagram -> TextDecision -> PostTextEdit -> TextData -> Diagram
handleTextDecision diagram decision postAction textData =
    case decision of 
        Accept ->
            ( case postAction of
                StoreEventText eventLine ->
                    { diagram | events =
                        List.indexedMap (\i e ->
                            if i == eventLine then
                                textData.current
                            else
                                e
                        ) diagram.events
                    }
                StorePointText dimensionName point ->
                    changeDimension dimensionName diagram
                        (changePointInDimension ((==) point) (pointWithText textData.current))
                StoreTargetContextText ->
                    { diagram | targetContext = textData.current }
            ) |> removeInteractableUI      
        Cancel ->
            removeInteractableUI diagram

handleTextRemoval : TextRemoval -> TextData -> TextData
handleTextRemoval removal textData =
    case removal of
        DeleteLeft ->
            case textData.cursor of
                End ->
                    if String.length textData.current > 0 then
                        { textData | current = String.left (String.length textData.current - 1) textData.current }
                    else
                        textData
                Start ->
                    textData
                AfterIndex 0 ->
                    { textData
                    | current = String.dropLeft 1 textData.current
                    , cursor = Start
                    }
                AfterIndex n ->
                    { textData
                    | current = String.left n textData.current ++ String.dropLeft (n + 1) textData.current
                    , cursor = AfterIndex (n - 1)
                    }
        DeleteRight ->
            case textData.cursor of
                End ->
                    textData
                Start ->
                    { textData | current = String.dropLeft 1 textData.current }
                AfterIndex n ->
                    if n + 1 == String.length textData.current then
                        { textData
                        | current = String.left n textData.current
                        , cursor = End
                        }
                    else
                        { textData
                        | current = String.left (n + 1) textData.current ++ String.dropLeft (n + 2) textData.current
                        }

handleTextCursor : TextCursorChange -> TextData -> TextData
handleTextCursor cursorChange textData =
    case cursorChange of
        CursorToStart ->
            if String.length textData.current == 0 then
                { textData | cursor = End }
            else
                { textData | cursor = Start }
        CursorToEnd ->
            { textData | cursor = End }
        CursorLeft ->
            case textData.cursor of
                End ->
                    if String.length textData.current == 0 then
                        { textData | cursor = End }
                    else if String.length textData.current == 1 then
                        { textData | cursor = Start }
                    else
                        { textData | cursor = AfterIndex <| String.length textData.current - 1 }
                Start ->
                    textData
                AfterIndex 0 ->
                    { textData | cursor = Start }
                AfterIndex n ->
                    { textData | cursor = AfterIndex (n - 1) }
        CursorRight ->
            case textData.cursor of
                End ->
                    textData
                Start ->
                    { textData | cursor = AfterIndex 0 }
                AfterIndex n ->
                    if n + 1 == String.length textData.current then
                        { textData | cursor = End }
                    else
                        { textData | cursor = AfterIndex (n + 1) }

handleTextInput : String -> TextData -> TextData
handleTextInput s textData =
    if String.length textData.current >= textData.maxLength then
        textData
    else
        case textData.cursor of
            End ->
                { textData | current = textData.current ++ s }
            Start ->
                { textData
                | current = s ++ textData.current
                , cursor = AfterIndex 0
                }
            AfterIndex n ->
                { textData
                | current = String.left (n + 1) textData.current ++ s ++ String.dropLeft (n + 1) textData.current
                , cursor = AfterIndex (n + 1)
                }

handleTextAction : Diagram -> TextAction -> PostTextEdit -> TextData -> Diagram
handleTextAction diagram textAction postAction textData =
    case textAction of
        Decision decision ->
            handleTextDecision diagram decision postAction textData
        Removal removal ->
            showTextUI diagram (handleTextRemoval removal textData) postAction
        CursorChange cursorChange ->
            showTextUI diagram (handleTextCursor cursorChange textData) postAction
        Key key ->
            showTextUI diagram (handleTextInput key textData) postAction

updateText : Diagram -> TextAction -> Diagram
updateText diagram textAction =
    case diagram.ux.interactable of
        Just ( InteractableText textData postAction ) ->
            handleTextAction diagram textAction postAction textData
        _ ->
            diagram

pointToText : Point -> String
pointToText point =
    case point of
        Value _ _ ->
            ""
        Described _ _ s ->
            s

editPointText : Diagram -> Point -> Diagram
editPointText diagram point =
    withFocusedDimension diagram diagram.dimensions (\diagram_ dimensionName _ ->
            showTextUI
                diagram_
                { current = pointToText point, cursor = End, maxLength = 150 }
                (StorePointText dimensionName point)        
    )

encodeDiagramFile : Diagram -> Json.Encode.Value
encodeDiagramFile diagram =
    Serialize.Encode.encodeDiagram diagram

saveFile : Json.Encode.Value -> Cmd a
saveFile value =
    Json.Encode.encode 0 value
    |> Download.string "lct-diagram.json" "application/json"

saveDiagramFile : Diagram -> Cmd a
saveDiagramFile diagram =
    encodeDiagramFile diagram
    |> saveFile

selectJsonFile : Cmd Message
selectJsonFile =
    Select.file ["application/json"] (Load << Select)

getFileContent : File -> Cmd Message
getFileContent file =
    Task.perform (Load << Loaded) (File.toString file)

loadDiagram : String -> Diagram -> Diagram
loadDiagram content diagram =
    case Json.Decode.decodeString Serialize.Decode.diagram content of
        Err _ ->
            diagram
        Ok decoded ->
            decoded

showGaze : AdditionalInfo -> AdditionalInfo
showGaze info =
    { info | showGaze = True }

showEpistemic : AdditionalInfo -> AdditionalInfo
showEpistemic info =
    { info | showEpistemic = True }

showInferredLine : InferredLine -> Diagram -> Diagram
showInferredLine info diagram =
    case info of
        Gaze ->
            { diagram | additionalInfo = showGaze diagram.additionalInfo }
        Epistemic ->
            { diagram | additionalInfo = showEpistemic diagram.additionalInfo }

hideGaze : AdditionalInfo -> AdditionalInfo
hideGaze info =
    { info | showGaze = False }

hideEpistemic : AdditionalInfo -> AdditionalInfo
hideEpistemic info =
    { info | showEpistemic = False }

hideInferredLine : InferredLine -> Diagram -> Diagram
hideInferredLine info diagram =
    case info of
        Gaze ->
            { diagram | additionalInfo = hideGaze diagram.additionalInfo }
        Epistemic ->
            { diagram | additionalInfo = hideEpistemic diagram.additionalInfo }

closeMenu : Diagram -> Diagram
closeMenu diagram =
    modifyUX diagram (\ux -> { ux | menuShown = False })

openMenu : Diagram -> Diagram
openMenu diagram =
    modifyUX diagram (\ux -> { ux | menuShown = True })

editTargetContext : Diagram -> Diagram
editTargetContext diagram =
    showTextUI
        diagram
        { maxLength = 65
        , current = diagram.targetContext
        , cursor = End
        }
        StoreTargetContextText

update : Message -> Diagram -> (Diagram, Cmd Message)
update message diagram =
    case message of
        CloseMenu ->
            ( closeMenu diagram , Cmd.none )
        OpenMenu ->
            ( openMenu diagram , Cmd.none )
        FocusOn dimension ->
            ( focusOn dimension diagram , Cmd.none )
        DefocusOn dimension ->
            ( defocusOn dimension diagram, Cmd.none )
        ShowDimension dimension ->
            ( reshow dimension diagram, Cmd.none )
        HideInferred inf ->
            ( hideInferredLine inf diagram, Cmd.none )
        ShowInferred inf ->
            ( showInferredLine inf diagram, Cmd.none )
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
            ( interactivelyChangeDimensionOnDiagram diagram (deletePoint p) |> removeInteractableUI, Cmd.none )
        DoWithPoint EditPointText p ->
            ( editPointText diagram p, Cmd.none )
        RemovePointUI ->
            ( removeInteractableUI diagram, Cmd.none )
        DoWithEvent InsertEventAfter n ->
            ( insertEventAfter diagram n, Cmd.none )
        DoWithEvent DeleteEvent n ->
            ( deleteEvent diagram n, Cmd.none )
        DoWithEvent (EditEventText textData) n ->
            ( showTextUI diagram textData (StoreEventText n), Cmd.none )
        EditTargetContext ->
            ( editTargetContext diagram , Cmd.none )
        UpdateText textAction ->
            ( updateText diagram textAction, Cmd.none )
        Save diagramToSave ->
            ( diagram, saveDiagramFile diagramToSave )
        Load RequestLoad ->
            ( diagram, selectJsonFile )
        Load (Select file) ->
            ( diagram, getFileContent file )
        Load ( Loaded content) ->
            ( loadDiagram content diagram, Cmd.none )
        

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

drawPoint : Diagram -> HexColor -> DimensionUX -> Point -> Svg a
drawPoint diagram color ux point =
    case point of
        Value n _ ->
            circle
                [ cx (fromFloat (eventLineToGraphX diagram n))
                , cy (fromFloat (pointToGraphY diagram point))
                , r "5"
                , fillOpacity
                    (case ux of
                        Defocused ->
                            "0.1"
                        _ ->
                            "0.8"
                    )
                , fill color
                , stroke "black"
                , strokeOpacity
                    (case ux of
                        Defocused ->
                            "0.2"
                        _ ->
                            "1.0"
                    )
                ]
                []
        Described n _ description ->
            rect
                [ x (fromFloat (eventLineToGraphX diagram n - 5))
                , y (fromFloat (pointToGraphY diagram point - 5))
                , width "10"
                , height "10"
                , fillOpacity
                    (case ux of
                        Defocused ->
                            "0.1"
                        _ ->
                            "0.8"
                    )
                , fill color
                , stroke "black"
                , strokeOpacity
                    (case ux of
                        Defocused ->
                            "0.2"
                        _ ->
                            "1.0"
                    )
                ]
                [ Svg.title [] [ text description ] ]

drawContinuousLine : Diagram -> HexColor -> List (Attribute a) -> List ( Float, Float ) -> Svg a
drawContinuousLine diagram color extraAttributes coordinates =
    case coordinates of
        [] ->
            g [] []
        [_] ->
            g [] []
        (ix, iy)::_ ->
            Svg.path
                ([ d
                    ( List.foldl (\(x, y) state ->
                        state
                        ++ fromFloat (x - (toFloat diagram.config.eventSpacing / 3)) ++ " " ++ fromFloat y ++ ", "
                        ++ fromFloat x ++ " " ++ fromFloat y ++ " "
                    ) ("M " ++ fromFloat ix ++ " " ++ fromFloat iy ++ " S ") coordinates
                    )
                , stroke color
                , strokeWidth "2"
                , fill "transparent"
                ] ++ extraAttributes)
                []

drawLine : Diagram -> HexColor -> DimensionUX -> List Point -> Svg a
drawLine diagram color ux =
    let
        extraAttrs =
                [ strokeOpacity
                    (case ux of
                        Defocused ->
                            "0.1"
                        _ ->
                            "1.0"
                    )
                ]
    in
        List.sortBy ( pointToEventLine )
        >> List.map ( pointToGraphCoordinates diagram )
        >> ( drawContinuousLine diagram color extraAttrs )

drawPoints : Diagram -> HexColor -> DimensionUX -> List Point -> Svg a
drawPoints diagram color ux points =
    g
        []
        ( List.map (drawPoint diagram color ux) points )

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
                        [ text "Continue with same strength" ]
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
                        [ text ("Stronger: " ++ dimension.plus) ]
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
                        [ text ("Weaker: " ++ dimension.minus) ]
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
                , onClick (DoWithPoint EditPointText point) 
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
            , drawPoint diagram dimension.color dimension.ux point
            ]
    )

countSpacesOnRight : String -> Int
countSpacesOnRight s =
    case String.right 1 s of
        " " ->
            1 + countSpacesOnRight (String.left (String.length s - 1) s)
        _ ->
            0

drawInteractableText : Diagram -> TextData -> Svg Message
drawInteractableText diagram textData =
    g
        []
        [ rect
            [ x (fromFloat (dia_withGraphWidth diagram (\w -> w / 2 - 160)))
            , y (fromFloat (dia_withGraphHeight diagram (\h -> h / 2 - 70)))
            , width (fromFloat 320)
            , height "140"
            , rx "4"
            , fill "#bbba"
            ]
            []
        , rect
            [ x (fromFloat (dia_withGraphWidth diagram (\w -> w / 2 - 150)))
            , y (fromFloat (dia_withGraphHeight diagram (\h -> h / 2 - 60)))
            , width (fromFloat 300)
            , height "120"
            , rx "4"
            , fill "white"
            , id "text-input"
            ]
            []
        , foreignObject 
            [ x (fromFloat (dia_withGraphWidth diagram (\w -> w / 2 - 148)))
            , y (fromFloat (dia_withGraphHeight diagram (\h -> h / 2 - 60)))
            , width "300"
            , height "100"
            ]
            [ Html.div
                [ Html.Attributes.style "font-family" "Calibri,sans-serif"
                , Html.Attributes.style "font-size" "12pt"
                ]
                (
                    let
                        cursor =
                            Html.span
                                [ Html.Attributes.style "position" "absolute"
                                , Html.Attributes.style "margin-top" "-2px"
                                , Html.Attributes.style "margin-left" "-2px"
                                ]
                                [ text "|" ]
                    in
                    case textData.cursor of
                        Start ->
                            [ cursor
                            , Html.span [] [ text textData.current ]
                            ]
                        End ->
                            [ Html.span [] [ text textData.current ]
                            , Html.span
                                []
                                [ text <| String.repeat (countSpacesOnRight textData.current) "\u{00A0}" ] -- non-breaking space
                            , cursor
                            ]
                        AfterIndex n ->
                            [ Html.span [] [ text (String.left (n + 1) textData.current) ]
                            , cursor
                            , Html.span [] [ text (String.dropLeft (n + 1) textData.current) ]
                            ]
                )
            ]
        ]

drawInteractable : Diagram -> Maybe Interactable -> Svg Message
drawInteractable diagram interactable =
    case interactable of
        Nothing ->
            g [] []
        Just (InteractablePoint point) ->
            withFocusedDimension
                (g [] [])
                diagram.dimensions
                (\_ _ dimension ->
                    drawPointInteractionUI diagram dimension point
                )
        Just (InteractableText textData _) ->
            drawInteractableText diagram textData

sortByFocused : Diagram -> List Dimension -> List Dimension
sortByFocused diagram dimensions =
    List.sortBy
        (\dimension ->
            case dimension.ux of
                Focused ->
                    2
                Defocused ->
                    1
                Shown ->
                    0
        )
        dimensions

drawDimensionsLines : Diagram -> Svg a
drawDimensionsLines diagram =
    g
        []
        ( Dict.values diagram.dimensions
          |> sortByFocused diagram
          |> List.map (\dimension -> drawLine diagram dimension.color dimension.ux dimension.points)
        )

drawDimensionsPoints : Diagram -> Svg a
drawDimensionsPoints diagram =
    g
        []
        ( Dict.values diagram.dimensions
          |> sortByFocused diagram
          |> List.map (\dimension -> drawPoints diagram dimension.color dimension.ux dimension.points)
        )

inferredLineToString : InferredLine -> String
inferredLineToString ai =
    case ai of
        Gaze ->
            "Gaze"
        Epistemic ->
            "Epistemic Relation"

drawInferredButton : Diagram -> Int -> HexColor -> InferredLine -> Bool -> Svg Message
drawInferredButton diagram index color inferred currentlyShown =
    g
        [ (if currentlyShown then
            onClick (HideInferred inferred)
          else
            onClick (ShowInferred inferred)
          )
        , Svg.Attributes.cursor "pointer"
        ]
        [ rect
            [ x (fromFloat (dia_withGraphWidth diagram (\w -> w - 180)))
            , y (fromFloat (dia_withGraphHeight diagram (\h -> h - (30 * toFloat index)) - 34))
            , width "170"
            , height "24"
            , rx "4"
            , fillOpacity
                ( if currentlyShown then
                    "0.8"
                  else
                    "0.2"
                )
            , fill color
            , stroke "transparent"
            , strokeWidth "1"
            ]
            []
        , text_
            [ x (fromFloat (dia_withGraphWidth diagram (\w -> w - 180) + 5))
            , y (fromFloat (dia_withGraphHeight diagram (\h -> h - (30 * toFloat index) - 17)))
            , stroke "white"
            , strokeWidth "4"
            , strokeOpacity "0.4"
            , fillOpacity "0.0"
            , fontFamily "Calibri, sans-serif"
            , fontSize "14pt"
            ]
            [ text (inferredLineToString inferred) ]
        , text_
            [ x (fromFloat (dia_withGraphWidth diagram (\w -> w - 180) + 5))
            , y (fromFloat (dia_withGraphHeight diagram (\h -> h - (30 * toFloat index) - 17)))
            , fill "black"
            , fillOpacity
                ( if currentlyShown then
                    "1.0"
                  else
                    "0.2"
                )
            , strokeOpacity "0.0"
            , fontFamily "Calibri, sans-serif"
            , fontSize "14pt"
            ]
            [ text (inferredLineToString inferred) ]
        ]

drawInferredButtons : Diagram -> Svg Message
drawInferredButtons diagram =
    g
        []
        [ drawInferredButton diagram 0 "#f58231" Gaze diagram.additionalInfo.showGaze
        , drawInferredButton diagram 1 "#774411" Epistemic diagram.additionalInfo.showEpistemic
        ]

interpolateLinePoints : List Point -> List Point
interpolateLinePoints points =
    window 2 points
    |> List.concatMap
        (\subPoints ->
            case subPoints of
                a::b::[] ->
                    let
                        evtLineA = pointToEventLine a
                        evtLineB = pointToEventLine b
                        numToCalculate = evtLineB - evtLineA - 1
                        valueA = pointToQuantitativeValue a
                        valueB = pointToQuantitativeValue b
                        valueBump = ((valueA + valueB) / 2.0) / toFloat numToCalculate
                    in
                        -- each point is evenly spaced
                        List.range (evtLineA + 1) (evtLineB - 1)
                        |> List.indexedMap (\i n ->
                            Value n (valueBump * (toFloat i+1))
                        )
                _ ->
                    []
        )
    |> (++) points
    |> List.sortBy pointToEventLine

inferredPointsToCoordinates : Diagram -> List Point -> List (Float, Float)
inferredPointsToCoordinates diagram =
    List.sortBy ( pointToEventLine )
    >> interpolateLinePoints
    >> List.map ( pointToGraphCoordinates diagram )

drawInferredLine : Diagram -> HexColor -> Dimension -> Dimension -> Svg a
drawInferredLine diagram color dimA dimB =
    let
        inferredA = inferredPointsToCoordinates diagram dimA.points
        inferredB = inferredPointsToCoordinates diagram dimB.points
        newLineLength =
             Basics.min (List.length inferredA) (List.length inferredB)
        produced =
            List.map2
                (\(x, aY) (_, bY) ->
                    (x, (aY + bY) / 2.0)
                )
                (List.take newLineLength inferredA)
                (List.take newLineLength inferredB)
        extraAttrs =
            [ strokeDasharray "4 1"
            ]
    in
        drawContinuousLine diagram color extraAttrs produced

drawInferredLines : Diagram -> Svg a
drawInferredLines diagram =
    g
        []
        [ if diagram.additionalInfo.showGaze then
            Maybe.map2
                (drawInferredLine diagram "#f58231")
                (dictGet SubR diagram.dimensions)
                (dictGet IR diagram.dimensions)
            |> Maybe.withDefault (g [id "SubR or IR not found! Error somewhere ????"] [])
          else
            g [] []
        , if diagram.additionalInfo.showEpistemic then
            Maybe.map2
                (drawInferredLine diagram "#774411")
                (dictGet OR diagram.dimensions)
                (dictGet DR diagram.dimensions)
            |> Maybe.withDefault (g [id "OR or DR not found! Error somewhere ????"] [])
          else
            g [] []
        ]

drawFocusButton : Diagram -> Int -> HexColor -> DimensionName -> Dimension -> Svg Message
drawFocusButton diagram index color dimensionName dimension =
    g
        [ (case dimension.ux of
            Focused ->
                onClick (DefocusOn dimensionName)
            Defocused ->
                onClick (ShowDimension dimensionName)
            Shown ->
                onClick (FocusOn dimensionName)
          )
        , Svg.Attributes.cursor "pointer"
        ]
        [ rect
            [ x (fromFloat (dia_withGraphWidth diagram (\w -> w - 180)))
            , y (fromFloat (dia_withGraphY diagram (\y -> y + (30 * toFloat index) + 10)))
            , width "170"
            , height "24"
            , rx "4"
            , fillOpacity
                ( if dimension.ux == Defocused then
                    "0.2"
                  else
                    "0.8"
                )
            , fill color
            , stroke
                ( if dimension.ux == Focused then
                    "black"
                  else
                    "transparent"
                )
            , strokeWidth
                ( if dimension.ux == Focused then
                    "2"
                  else
                    "1"
                )
            ]
            []
        , text_
            [ x (fromFloat (dia_withGraphWidth diagram (\w -> w - 180) + 5))
            , y (fromFloat (dia_withGraphY diagram (\y -> y + (30 * toFloat index) + 27)))
            , stroke "white"
            , strokeWidth "4"
            , strokeOpacity "0.4"
            , fillOpacity "0.0"
            , fontFamily "Calibri, sans-serif"
            , fontSize "14pt"
            ]
            [ text (dimensionNameToString dimensionName) ]
        , text_
            [ x (fromFloat (dia_withGraphWidth diagram (\w -> w - 180) + 5))
            , y (fromFloat (dia_withGraphY diagram (\y -> y + (30 * toFloat index) + 27)))
            , fill "black"
            , fillOpacity
                ( if dimension.ux == Defocused then
                    "0.2"
                  else
                    "1.0"
                )
            , fontFamily "Calibri, sans-serif"
            , fontSize "14pt"
            ]
            [ text (dimensionNameToString dimensionName) ]
        ]

drawFocusButtons : Diagram -> Svg Message
drawFocusButtons diagram =
    g
        []
        ( Dict.toList diagram.dimensions
          |> List.indexedMap
                (\i (dimensionName, dimension) ->
                    case dimensionNameToDimension diagram dimensionName of
                        Just dim ->
                            drawFocusButton diagram i dim.color dimensionName dimension
                        Nothing ->
                            g [] []
                )
        )

drawLoadSave : Diagram -> Svg Message
drawLoadSave diagram =
    g
        []
        [ g -- group for burger-menu
            [ onClick
                (if diagram.ux.menuShown then
                    CloseMenu
                 else
                    OpenMenu
                )
            , Svg.Attributes.cursor "pointer"
            ]
            [ rect
                [ x "5"
                , y "5"
                , width "32"
                , height "25"
                , fill "white"
                , rx "2"
                ]
                []
            , if diagram.ux.menuShown then
                g
                    [ transform ("translate (14 5) scale (0.05)")
                    ]
                    [ viewIcon FontAwesome.Solid.ellipsisV ]
              else
                g
                    [ transform ("translate (5 5) scale (0.05)")
                    ]
                    [ viewIcon FontAwesome.Solid.ellipsisH ]
            ]
        , if diagram.ux.menuShown then
            g
                []
                [ g -- group for save-button
                    [ onClick (Save diagram)
                    , Svg.Attributes.cursor "pointer"
                    ]
                    [ rect
                        [ x "5"
                        , y "40"
                        , width "32"
                        , height "25"
                        , fill "white"
                        , rx "2"
                        ]
                        []
                    , g
                        [ transform ("translate (5 40) scale (0.05)")
                        ]
                        [ viewIcon FontAwesome.Solid.cloudDownloadAlt ]
                    ]
                , g -- group for load-button
                    [ onClick (Load RequestLoad)
                    , Svg.Attributes.cursor "pointer"
                    ]
                    [ rect
                        [ x "5" 
                        , y "75" 
                        , width "32"
                        , height "25"
                        , fill "white"
                        , rx "2"
                        ]
                        []
                    , g
                        [ transform ("translate (8 75) scale (0.05)")
                        ]
                        [ viewIcon FontAwesome.Solid.upload ]
                    ]
                ]
          else
            g [] []
        ]

drawTargetContext : Diagram -> Svg Message
drawTargetContext diagram =
    g
        []
        [ text_
            [ x "65"
            , y "25"
            , fontFamily "Times New Roman, serif"
            , fontSize "20pt"
            , fontWeight "bold"
            ]
            [ text diagram.targetContext ]
        , g
            [ onClick EditTargetContext
            , Svg.Attributes.cursor "pointer"
            ]
            [ rect
                [ x "40"
                , y "5"
                , width "25"
                , height "25"
                , fill "white"
                , rx "2"
                ]
                []
            , g
                [ transform ("translate (40 7) scale (0.04)")
                ]
                [ viewIcon FontAwesome.Solid.edit ]
            ]

        ]


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
    , drawFocusButtons diagram
    , drawEvents diagram
    , drawInferredLines diagram
    , drawDimensionsLines diagram
    , drawDimensionsPoints diagram
    , drawInferredButtons diagram
    , drawLoadSave diagram
    , drawTargetContext diagram
    , drawInteractable diagram diagram.ux.interactable
    ]

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

busyWithTextInteraction : Diagram -> Bool
busyWithTextInteraction diagram =
    case diagram.ux.interactable of
        Just (InteractableText _ _) ->
            True
        _ ->
            False

withinPointRadius : Diagram -> D.Decoder Message
withinPointRadius diagram =
    if busyWithTextInteraction diagram then
        D.fail "In the middle of a text interaction - ignoring point interactions for now"
    else
        withFocusedDimension
            (D.fail "No focused dimension")
            diagram.dimensions
            (\_ _ dimension ->
                D.map2 (\x y -> (x, y))
                    (D.field "clientX" D.int)
                    (D.field "clientY" D.int)
                |> D.andThen
                    (\coords ->
                        case pointWithinRadius diagram 35 coords dimension.points of
                            Just point ->
                                D.succeed (DoWithPoint ShowPointUI point)
                            Nothing ->
                                case diagram.ux.interactable of
                                    Just (InteractablePoint _) ->
                                        -- I am outside the radius.
                                        -- No matter what the Point interaction is, it dies now.
                                        D.succeed RemovePointUI
                                    Just (InteractableText _ _) ->
                                        D.fail "In the middle of a text interaction - ignoring point interactions for now"
                                    Nothing ->
                                        D.fail "No point within radius"
                    )
            )

keyToTextAction : String -> Maybe TextAction
keyToTextAction s =
    let
        specialKeys =
            ElmDict.fromList
                [ ( "Home", CursorChange CursorToStart )
                , ( "End", CursorChange CursorToEnd )
                , ( "ArrowRight", CursorChange CursorRight )
                , ( "ArrowLeft", CursorChange CursorLeft )
                , ( "Right", CursorChange CursorRight )
                , ( "Left", CursorChange CursorLeft )
                , ( "Backspace", Removal DeleteLeft )
                , ( "Delete", Removal DeleteRight )
                , ( "Accept", Decision Accept )
                , ( "Enter", Decision Accept )
                , ( "Finish", Decision Accept )
                , ( "Save", Decision Accept )
                , ( "Cancel", Decision Cancel )
                , ( "Escape", Decision Cancel )
                , ( "GoBack", Decision Cancel )
                , ( "Close", Decision Cancel )
                ]
    in
        if String.length s == 1 then
            Just (Key s)
        else
            ElmDict.get s specialKeys
    

interpretKeypress : Diagram -> D.Decoder Message
interpretKeypress diagram =
    case diagram.ux.interactable of
        Just (InteractableText _ _) ->
            D.map4 (\ctrl alt composing meta -> ctrl || alt || composing || meta)
                (D.field "altKey" D.bool)
                (D.field "ctrlKey" D.bool)
                (D.field "isComposing" D.bool)
                (D.field "metaKey" D.bool)
            |> D.andThen (\shouldIgnore ->
                if shouldIgnore then
                    D.fail <| "Ctrl, Alt, Composing, or Meta is active.  Ignoring."
                else
                    D.field "key" D.string
                    |> D.andThen (\key ->
                        case keyToTextAction key of
                            Nothing ->
                                D.fail ("No matching key for input: " ++ key)
                            Just textAction ->
                                D.succeed (UpdateText textAction)
                    )
            )
        _ ->
            D.fail <| "No text-editing interaction active"

view : Diagram -> Html Message
view diagram =
    Html.div
        [ Svg.Events.on "mousemove" (withinPointRadius diagram)            
        ]
        [ svgView diagram ]

subscriptions : Diagram -> Sub Message
subscriptions diagram =
    case diagram.ux.interactable of
        Just ( InteractableText _ _ ) ->
            Sub.batch
                [ Browser.Events.onKeyDown (interpretKeypress diagram)
                ]
        _ ->
            Sub.none

main : Program () Diagram Message
main =
    Browser.element
        { init = \_ -> (init, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }