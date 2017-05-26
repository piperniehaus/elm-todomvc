module TestHelpers exposing (..)

import Types exposing (..)


emptyModel : Model
emptyModel =
    { entries = []
    , field = ""
    , uid = 0
    , visibility = "All"
    }


incompleteEntry : Int -> Entry
incompleteEntry id =
    { description = "Incomplete Entry"
    , completed = False
    , editing = False
    , id = id
    }


completeEntry : Int -> Entry
completeEntry id =
    { description = "Complete Entry"
    , completed = True
    , editing = False
    , id = id
    }


entries2 : List Entry
entries2 =
    [ incompleteEntry 1, completeEntry 2 ]


modelWithTwoEntries : Model
modelWithTwoEntries =
    { emptyModel | uid = 3, entries = entries2 }
