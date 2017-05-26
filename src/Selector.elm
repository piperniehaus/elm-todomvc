module Selector exposing (selector, ViewModel)

import Types exposing (..)


type alias ViewModel =
    { model : Model
    , entriesLeft : Int
    , entriesCompleted : Int
    , allCompleted : Bool
    , visibleEntries : List Entry
    }


selector : Model -> ViewModel
selector model =
    { model = model
    , entriesLeft = entriesLeft model
    , entriesCompleted = entriesCompleted model
    , allCompleted = allCompleted model
    , visibleEntries = visibleEntries model
    }


entriesLeft : Model -> Int
entriesLeft ({ entries } as model) =
    List.length entries - entriesCompleted model


entriesCompleted : Model -> Int
entriesCompleted { entries } =
    entries
        |> List.filter .completed
        |> List.length


visibleEntries : Model -> List Entry
visibleEntries { entries, visibility } =
    let
        isVisible todo =
            case visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    True
    in
        List.filter isVisible entries


allCompleted : Model -> Bool
allCompleted { entries } =
    List.all .completed entries
