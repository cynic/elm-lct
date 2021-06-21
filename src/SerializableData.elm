module SerializableData exposing (..)
import GenericDict as Dict exposing (Dict)

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

type alias RangeDescription =
    { description : String
    , range : ( Float, Float )
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

type alias Dimension =
    { texts : List RangeDescription
    , points : List Point
    , plus : String
    , minus : String
    , color : String
    }

type DimensionName
    = SG
    | SD
    | Gaze
    | OR
    | DR

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

type PostTextEdit
    = StoreEventText EventLine
    | StorePointText DimensionName Point

type Interactable
    = InteractablePoint Point
    | InteractableText TextData PostTextEdit

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
