module Suites.Main exposing (all)

import Test exposing (..)
import Expect
import Main exposing (..)
import TestHelpers exposing (..)
import Types exposing (..)


all : Test
all =
    describe "Main"
        [ describe "update"
            [ describe "Add"
                [ describe "when the field is blank" <|
                    let
                        ( actualModel, _ ) =
                            update Add modelWithTwoEntries
                    in
                        [ test "does not change entries" <|
                            \_ ->
                                actualModel.entries
                                    |> Expect.equal modelWithTwoEntries.entries
                        , test "increments uid" <|
                            \_ ->
                                let
                                    ( actualModel, _ ) =
                                        update Add modelWithTwoEntries
                                in
                                    actualModel.uid
                                        |> Expect.equal (modelWithTwoEntries.uid + 1)
                        ]
                ]
            , describe "when the field is not blank" <|
                let
                    field =
                        "DO THIS!"

                    ( actualModel, _ ) =
                        update Add { modelWithTwoEntries | field = field }
                in
                    [ test "adds entry" <|
                        \_ ->
                            let
                                expectedEntry =
                                    { description = field
                                    , completed = False
                                    , editing = False
                                    , id = modelWithTwoEntries.uid
                                    }

                                expectedEntries =
                                    List.append modelWithTwoEntries.entries [ expectedEntry ]
                            in
                                actualModel.entries
                                    |> Expect.equal expectedEntries
                    , test "increments uid" <|
                        \_ ->
                            actualModel.uid
                                |> Expect.equal (modelWithTwoEntries.uid + 1)
                    ]
            , describe "UpdateField"
                [ test "sets the field to the provided String" <|
                    \_ ->
                        let
                            string =
                                "My string"

                            ( actualModel, _ ) =
                                update (UpdateField string) emptyModel
                        in
                            actualModel.field
                                |> Expect.equal string
                ]
            , describe "EditingEntry"
                [ test "nothing changes when the id is not in the entries list" <|
                    \_ ->
                        let
                            ( actualModel, _ ) =
                                update (EditingEntry 42 True) modelWithTwoEntries
                        in
                            actualModel
                                |> Expect.equal modelWithTwoEntries
                , test "sets the entry editing when the id is in the entries list" <|
                    \_ ->
                        let
                            matchingEntry =
                                incompleteEntry 1

                            ( actualModel, _ ) =
                                update (EditingEntry matchingEntry.id True) modelWithTwoEntries
                        in
                            List.head actualModel.entries
                                |> Expect.equal (Just { matchingEntry | editing = True })
                ]
            , describe "UpdateEntry"
                [ test "nothing changes when the id is not in the entries list" <|
                    \_ ->
                        let
                            ( actualModel, _ ) =
                                update (UpdateEntry 42 "New task") modelWithTwoEntries
                        in
                            actualModel
                                |> Expect.equal modelWithTwoEntries
                , test "sets the entry description when the id is in the entries list" <|
                    \_ ->
                        let
                            matchingEntry =
                                incompleteEntry 1

                            ( actualModel, _ ) =
                                update (UpdateEntry matchingEntry.id "New task") modelWithTwoEntries
                        in
                            List.head actualModel.entries
                                |> Expect.equal (Just { matchingEntry | description = "New task" })
                ]
            , describe "Delete"
                [ test "nothing changes when the id is not in the entries list" <|
                    \_ ->
                        let
                            ( actualModel, _ ) =
                                update (Delete 42) modelWithTwoEntries
                        in
                            actualModel
                                |> Expect.equal modelWithTwoEntries
                , test "deletes the entry when the id is in the entries list" <|
                    \_ ->
                        let
                            matchingEntry =
                                incompleteEntry 1

                            ( actualModel, _ ) =
                                update (Delete matchingEntry.id) modelWithTwoEntries
                        in
                            actualModel.entries
                                |> Expect.equal [ completeEntry 2 ]
                ]
            , describe "DeleteComplete"
                [ test "deletes all complete entries" <|
                    \_ ->
                        let
                            ( actualModel, _ ) =
                                update DeleteComplete modelWithTwoEntries
                        in
                            actualModel.entries
                                |> Expect.equal [ incompleteEntry 1 ]
                ]
            , describe "Check"
                [ test "nothing changes when the id is not in the entries list" <|
                    \_ ->
                        let
                            ( actualModel, _ ) =
                                update (Check 42 True) modelWithTwoEntries
                        in
                            actualModel
                                |> Expect.equal modelWithTwoEntries
                , test "sets the entry completion when the id is in the entries list" <|
                    \_ ->
                        let
                            matchingEntry =
                                incompleteEntry 1

                            ( actualModel, _ ) =
                                update (Check matchingEntry.id True) modelWithTwoEntries
                        in
                            List.head actualModel.entries
                                |> Expect.equal (Just { matchingEntry | completed = True })
                ]
            , describe "CheckAll"
                [ test "checks all entries" <|
                    \_ ->
                        let
                            ( actualModel, _ ) =
                                update (CheckAll True) modelWithTwoEntries
                        in
                            actualModel.entries
                                |> List.all .completed
                                |> Expect.equal True
                ]
            , describe "ChangeVisibility"
                [ test "updates the visibility" <|
                    \_ ->
                        let
                            visibilityString =
                                "Invisibility cloak"

                            ( actualModel, _ ) =
                                update (ChangeVisibility visibilityString) emptyModel
                        in
                            actualModel.visibility
                                |> Expect.equal visibilityString
                ]
            ]
        ]
