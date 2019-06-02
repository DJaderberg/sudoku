module Tests exposing (suite)

import Board exposing (..)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Random
import Set exposing (fromList, size)
import Test exposing (..)


position : Fuzzer Position
position =
    Fuzz.map2 (\a b -> ( a, b )) (Fuzz.intRange 1 9) (Fuzz.intRange 1 9)


dummyBoard =
    List.foldl (\( x, y ) b -> insert ( x, y ) (10 * x + y) b) emptyBoard positions


suite : Test
suite =
    describe "The Sudoku module"
        [ describe "Numbering of boxes"
            [ test "(1, 1) is in box 1" <|
                \_ -> getBox ( 1, 1 ) |> Expect.equal 1
            , test "(4, 1) is in box 2" <|
                \_ -> getBox ( 4, 1 ) |> Expect.equal 2
            , test "(1, 6) is in box 4" <|
                \_ -> getBox ( 1, 6 ) |> Expect.equal 4
            , test "(3, 6) is in box 4" <|
                \_ -> getBox ( 3, 6 ) |> Expect.equal 4
            , test "(8, 8) is in box 9" <|
                \_ -> getBox ( 8, 8 ) |> Expect.equal 9
            , test "(9, 9) is in box 9" <|
                \_ -> getBox ( 9, 9 ) |> Expect.equal 9
            ]
        , describe "Options"
            [ fuzz position "All numbers OK for empty" <|
                \p -> options emptyBoard p |> Expect.equalSets (List.range 1 9 |> Set.fromList)
            , fuzz position "Position filled makes options singleton" <|
                \p -> options (insert p 9 emptyBoard) p |> Expect.equalSets (Set.singleton 9)
            , test "Element filled on row" <|
                \_ -> options (insert ( 1, 1 ) 9 emptyBoard) ( 1, 9 ) |> Expect.equalSets (List.range 1 8 |> Set.fromList)
            , test "Element filled on column" <|
                \_ -> options (insert ( 1, 1 ) 9 emptyBoard) ( 9, 1 ) |> Expect.equalSets (List.range 1 8 |> Set.fromList)
            , test "Element filled in box" <|
                \_ -> options (insert ( 1, 1 ) 9 emptyBoard) ( 3, 3 ) |> Expect.equalSets (List.range 1 8 |> Set.fromList)
            , test "Options on row" <|
                \_ ->
                    options
                        (emptyBoard |> insert ( 2, 1 ) 1)
                        ( 4, 1 )
                        |> Expect.equalSets (List.range 2 9 |> Set.fromList)
            , skip <|
                test "Options on row 2" <|
                    \_ ->
                        options
                            (emptyBoard |> insert ( 1, 1 ) 1 |> insert ( 2, 1 ) 2 |> insert ( 3, 1 ) 3)
                            ( 4, 1 )
                            |> Expect.equalSets (List.range 4 9 |> Set.fromList)
            ]
        , test "Row 1 of dummyBoard" <|
            \_ ->
                row dummyBoard 1
                    |> List.map (Maybe.withDefault 0)
                    |> Set.fromList
                    |> Expect.equalSets (Set.fromList [ 11, 12, 13, 14, 15, 16, 17, 18, 19 ])
        , test "Row 6 of dummyBoard" <|
            \_ ->
                row dummyBoard 6
                    |> List.map (Maybe.withDefault 0)
                    |> Set.fromList
                    |> Expect.equalSets (Set.fromList [ 61, 62, 63, 64, 65, 66, 67, 68, 69 ])
        , test "Column 9 of dummyBoard" <|
            \_ ->
                column dummyBoard 9
                    |> List.map (Maybe.withDefault 0)
                    |> Set.fromList
                    |> Expect.equalSets (Set.fromList [ 19, 29, 39, 49, 59, 69, 79, 89, 99 ])
        , test "Box 1 of dummyBoard" <|
            \_ ->
                box dummyBoard 1
                    |> List.map (Maybe.withDefault 0)
                    |> Set.fromList
                    |> Expect.equalSets (Set.fromList [ 11, 12, 13, 21, 22, 23, 31, 32, 33 ])
        , test "Box 5 of dummyBoard" <|
            \_ ->
                box dummyBoard 5
                    |> List.map (Maybe.withDefault 0)
                    |> Set.fromList
                    |> Expect.equalSets (Set.fromList [ 44, 45, 46, 54, 55, 56, 64, 65, 66 ])
        , test "Valid boards have 81 elements" <|
            \_ -> (validBoard |> Dict.toList |> List.length) |> Expect.equal 81
        , test "Rows have 9 elements" <|
            \_ -> (Board.row validBoard 4 |> List.length) |> Expect.equal 9
        , test "Columns have 9 elements" <|
            \_ -> (Board.column validBoard 4 |> List.length) |> Expect.equal 9
        , test "Rows have 9 different elements" <|
            \_ ->
                (Board.column validBoard 1 |> List.map (Maybe.withDefault 0) |> Set.fromList)
                    |> Expect.equalSets (Set.fromList (List.range 1 9))
        ]
