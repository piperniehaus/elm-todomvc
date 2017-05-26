module Suites.Selector exposing (all)

import Types exposing (..)
import Test exposing (..)
import Expect
import Selector exposing (..)
import Html exposing (div)
import TestHelpers exposing (..)


all : Test
all =
    describe "Selector"
        [ test "model" <|
            \_ ->
                selector emptyModel
                    |> .model
                    |> Expect.equal emptyModel
        , describe "entriesLeft"
            [ test "returns the total incomplete entries" <|
                \_ ->
                    selector { emptyModel | entries = entries2 }
                        |> .entriesLeft
                        |> Expect.equal 1
            ]
        , describe "entriesCompleted"
            [ test "returns the total complete entries" <|
                \_ ->
                    selector { emptyModel | entries = entries2 }
                        |> .entriesCompleted
                        |> Expect.equal 1
            ]
        , describe "allCompleted"
            [ test "returns True if all entries are completed" <|
                \_ ->
                    selector { emptyModel | entries = [ completeEntry 0, completeEntry 1 ] }
                        |> .allCompleted
                        |> Expect.equal True
            , test "returns False if all entries are completed" <|
                \_ ->
                    selector { emptyModel | entries = entries2 }
                        |> .allCompleted
                        |> Expect.equal False
            ]
        , describe "visibleEntries" <|
            let
                entries =
                    [ incompleteEntry 1, completeEntry 2 ]
            in
                [ test "shows all when visibility is \"All\"" <|
                    \_ ->
                        selector { emptyModel | visibility = "All", entries = entries }
                            |> .visibleEntries
                            |> Expect.equal entries
                , test "shows completed when visibility is \"Completed\"" <|
                    \_ ->
                        selector { emptyModel | visibility = "Completed", entries = entries }
                            |> .visibleEntries
                            |> Expect.equal [ completeEntry 2 ]
                , test "shows incomplete when visibility is \"Active\"" <|
                    \_ ->
                        selector { emptyModel | visibility = "Active", entries = entries }
                            |> .visibleEntries
                            |> Expect.equal [ incompleteEntry 1 ]
                , test "shows all when visibility is a random string" <|
                    \_ ->
                        selector { emptyModel | visibility = "asdf", entries = entries }
                            |> .visibleEntries
                            |> Expect.equal entries
                ]
        ]
