module Dimensions exposing (..)
import SerializableData exposing (..)

-- colors from https://sashamaps.net/docs/resources/20-colors/

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
    , color = "#e6194B" -- red
    , ux = Shown
    }

sdInit : Dimension
sdInit =
    { texts =
        []
    , points =
        [ Value 0 0.0 ]
    , plus = "Understanding requires a larger number of meanings per term; meaning condensed in jargon"
    , minus = "Understanding possible with fewer meanings per term; meaning expressed in \"ordinary\" terms"
    , color = "#f58231" -- orange
    , ux = Shown
    }

subrInit : Dimension
subrInit =
    { texts =
        []
    , points =
        [ Value 0 0.0 ]
    , plus = "Requires some inner or inborn skill or knowledge for true understanding"
    , minus = "Can be understood by a broad spectrum of people"
    , color = "#bfef45" -- lime
    , ux = Shown
    }


irInit : Dimension
irInit =
    { texts =
        []
    , points =
        [ Value 0 0.0 ]
    , plus = "Interactions with existing members and/or practices are necessary for true understanding"
    , minus = "Can be understood without adopting specific behaviours or having particular interactions"
    , color = "#3cb44b" -- mint
    , ux = Shown
    }

orInit : Dimension
orInit =
    { texts =
        []
    , points =
        [ Value 0 0.0 ]
    , plus = "Applicable problem-situations or objects-of-study are strongly delineated"
    , minus = "Applicable to any problem-situations or objects-of-study" 
    , color = "#42d4f4" -- cyan
    , ux = Shown
    }

drInit : Dimension
drInit =
    { texts =
        []
    , points =
        [ Value 0 0.0 ]
    , plus = "Legitimate approach to problem is strongly delineated"
    , minus = "Any approach to the problem is welcome"
    , color = "#4363d8" -- blue
    , ux = Shown
    }