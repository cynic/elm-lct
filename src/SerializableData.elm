module SerializableData exposing (..)
import GenericDict exposing (Dict)
import ListExtensions exposing (first)
import Set

type alias EventLine =
    Int

type alias Event =
    String

type alias MaxLength =
    Int

type CursorPosition
    = Start
    | End
    | AfterIndex Int

type alias TextData =
    { maxLength : MaxLength
    , current : String
    , cursor : CursorPosition
    }

type TextCursorChange
    = CursorToStart
    | CursorToEnd
    | CursorRight
    | CursorLeft

type TextRemoval
    = DeleteLeft
    | DeleteRight

type TextDecision
    = Accept
    | Cancel

type TextAction
    = CursorChange TextCursorChange
    | Removal TextRemoval
    | Decision TextDecision
    | Key String

type DimensionRange
    = PlusPlus
    | Plus
    | Minus
    | MinusMinus

type alias RangeDescription =
    { description : String
    , indicators : Set String
    , examples : Set String
    , range : DimensionRange
    }

type Point
    = Value EventLine Float
    | Described EventLine Float String

{- Oversimplified relationships:

🟣 OR: knowledge practices ↔ legitimate objects
🟣 DR: knowledge practices ↔ everything else
🟣 IR: knowledge practices ↔ ways of knowing
🟣 SubR: knowledge practices ↔ legitimate knowers
🟣 Gazes: knower ↔ knowledge ✅
🟣 SD: knowledge ↔ knowledge ✅
🟣 SG: context ↔ knowledge ✅
-}

type DimensionUX
    = Shown
    | Focused
    | Defocused

type alias Dimension =
    { texts : List RangeDescription
    , points : List Point
    , plus : String
    , minus : String
    , color : String
    , ux : DimensionUX
    }

type DimensionName
    = SG
    | SD
    | IR
    | SubR
    | OR
    | DR
    | PA
    | RA

type alias Band = Int -- which band (0-3) we're drawing

type alias HexColor = String

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
    | EditPointText

type EventInteraction
    = DeleteEvent
    | InsertEventAfter
    | EditEventText TextData

type TargetContextInteraction
    = EditTargetContextText TextData

type PostTextEdit
    = StoreEventText EventLine
    | StorePointText DimensionName Point
    | StoreTargetContextText

type Interactable
    = InteractablePoint Point
    | InteractableText TextData PostTextEdit

type alias UX =
    { interactable : Maybe Interactable
    , menuShown : Bool
    }

type alias AdditionalInfo =
    { showGaze : Bool
    , showEpistemic : Bool
    }

type alias Diagram =
    { textHeight : Int
    , width : Int
    , graphHeight : Int
    , additionalInfo : AdditionalInfo
    , events : List Event
    , dimensions : Dict DimensionName Dimension
    , config : Configuration
    , targetContext : String
    , ux : UX
    }

dimensionNameToString : DimensionName -> String
dimensionNameToString dim =
    case dim of
        SG ->
            "Semantic gravity"
        SD ->
            "Semantic density"
        IR ->
            "Interactional relation"
        SubR ->
            "Subjective relation"
        OR ->
            "Ontic relation"
        DR ->
            "Discursive relation"
        PA ->
            "Positional autonomy"
        RA ->
            "Relational autonomy"

-- we have a test for this, to make sure that we can round-trip.
stringToDimensionName : String -> Maybe DimensionName
stringToDimensionName s =
    case s of
        "Semantic gravity" ->
            Just SG
        "Semantic density" ->
            Just SD
        "Interactional relation" ->
            Just IR
        "Subjective relation" ->
            Just SubR
        "Ontic relation" ->
            Just OR
        "Discursive relation" ->
            Just DR
        "Positional autonomy" ->
            Just PA
        "Relational autonomy" ->
            Just RA
        _ ->
            Nothing
