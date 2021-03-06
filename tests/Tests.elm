module Tests exposing (suite)

import Board exposing (..)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Set
import Test exposing (..)


position : Fuzzer Position
position =
    Fuzz.map2 (\a b -> ( a, b )) (Fuzz.intRange 1 9) (Fuzz.intRange 1 9)


{-| An example board with easy to understand values
|11|21|31|41|51|61|71|81|91|
|12|22|32|42|52|62|72|82|92|
|13|23|33|43|53|63|73|83|93|
|14|24|34|44|54|64|74|84|94|
|15|25|35|45|55|65|75|85|95|
|16|26|36|46|56|66|76|86|96|
|17|27|37|47|57|67|77|87|97|
|18|28|38|48|58|68|78|88|98|
|19|29|39|49|59|69|79|89|99|
-}
dummyBoard =
    List.foldl (\( x, y ) b -> insert ( x, y ) (10 * x + y) b) empty positions


{-| Generate a random, filled board.
withDefault should never need to be used, but the type system doesn't know that
-}
board : Fuzzer Board
board =
    Fuzz.map
        (Random.initialSeed >> generate >> Maybe.withDefault empty)
        (Fuzz.intRange Random.minInt Random.maxInt)


suite : Test
suite =
    describe "The Board module"
        [ describe "Numbering of boxes"
            [ test "(1, 1) is in box 1" <|
                \_ ->
                    boxIndex ( 1, 1 )
                        |> Expect.equal 1
            , test "(4, 1) is in box 2" <|
                \_ ->
                    boxIndex ( 4, 1 )
                        |> Expect.equal 2
            , test "(1, 6) is in box 4" <|
                \_ ->
                    boxIndex ( 1, 6 )
                        |> Expect.equal 4
            , test "(3, 6) is in box 4" <|
                \_ ->
                    boxIndex ( 3, 6 )
                        |> Expect.equal 4
            , test "(8, 8) is in box 9" <|
                \_ ->
                    boxIndex ( 8, 8 )
                        |> Expect.equal 9
            , test "(9, 9) is in box 9" <|
                \_ ->
                    boxIndex ( 9, 9 )
                        |> Expect.equal 9
            ]
        , describe "Options to place in a position"
            [ fuzz position "All numbers OK for empty board" <|
                \p ->
                    options empty p
                        |> Expect.equalSets (List.range 1 9 |> Set.fromList)
            , test "Element filled on row" <|
                \_ ->
                    options (insert ( 1, 1 ) 9 empty) ( 1, 9 )
                        |> Expect.equalSets (List.range 1 8 |> Set.fromList)
            , test "Element filled on column" <|
                \_ ->
                    options (insert ( 1, 1 ) 9 empty) ( 9, 1 )
                        |> Expect.equalSets (List.range 1 8 |> Set.fromList)
            , test "Element filled in box" <|
                \_ ->
                    options (insert ( 1, 1 ) 9 empty) ( 3, 3 )
                        |> Expect.equalSets (List.range 1 8 |> Set.fromList)
            , test "Options on row" <|
                \_ ->
                    options
                        (empty |> insert ( 2, 1 ) 1)
                        ( 4, 1 )
                        |> Expect.equalSets (List.range 2 9 |> Set.fromList)
            , test "Options on row 2" <|
                \_ ->
                    options
                        (empty |> insert ( 1, 1 ) 1 |> insert ( 2, 1 ) 2 |> insert ( 3, 1 ) 3)
                        ( 4, 1 )
                        |> Expect.equalSets (List.range 4 9 |> Set.fromList)
            , test "Already set value is an option" <|
                \_ ->
                    options (empty |> insert ( 1, 1 ) 1) ( 1, 1 )
                        |> Expect.equalSets (Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])
            ]
        , test "Row 1 of dummyBoard is 11 to 19" <|
            \_ ->
                row dummyBoard 1
                    |> List.map (Maybe.withDefault 0)
                    |> Set.fromList
                    |> Expect.equalSets (Set.fromList [ 11, 12, 13, 14, 15, 16, 17, 18, 19 ])
        , test "Row 6 of dummyBoard is 61 to 69" <|
            \_ ->
                row dummyBoard 6
                    |> List.map (Maybe.withDefault 0)
                    |> Set.fromList
                    |> Expect.equalSets (Set.fromList [ 61, 62, 63, 64, 65, 66, 67, 68, 69 ])
        , test "Column 9 of dummyBoard is 91 to 99" <|
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
        , describe "Generated boards"
            [ fuzz board "Have 81 elements" <|
                \b -> (b |> Dict.toList |> List.length) |> Expect.equal 81
            , fuzz board "Rows have 9 elements" <|
                \b ->
                    List.range 1 9
                        |> List.map (Board.row b >> List.length)
                        |> Expect.equalLists [ 9, 9, 9, 9, 9, 9, 9, 9, 9 ]
            , fuzz board "Columns have 9 elements" <|
                \b ->
                    List.range 1 9
                        |> List.map (Board.column b >> List.length)
                        |> Expect.equalLists [ 9, 9, 9, 9, 9, 9, 9, 9, 9 ]
            , fuzz board "Boxes have 9 elements" <|
                \b ->
                    List.range 1 9
                        |> List.map (Board.box b >> List.length)
                        |> Expect.equalLists [ 9, 9, 9, 9, 9, 9, 9, 9, 9 ]
            , fuzz board "Rows have 9 different elements" <|
                \b ->
                    List.range 1 9
                        |> List.map (Board.row b >> List.filterMap identity >> List.sort)
                        |> Expect.equalLists (List.repeat 9 [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])
            , fuzz board "Columns have 9 different elements" <|
                \b ->
                    List.range 1 9
                        |> List.map (Board.column b >> List.filterMap identity >> List.sort)
                        |> Expect.equalLists (List.repeat 9 [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])
            , fuzz board "Boxes have 9 different elements" <|
                \b ->
                    List.range 1 9
                        |> List.map (Board.box b >> List.filterMap identity >> List.sort)
                        |> Expect.equalLists (List.repeat 9 [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])
            ]
        , test "Everything valid on empty board" <|
            \_ ->
                valid empty ( 1, 1 ) |> Expect.true "(1,1) is OK on empty board"
        , test "Valid on partially filled board" <|
            \_ ->
                valid (empty |> insert ( 1, 1 ) 1) ( 1, 1 )
                    |> Expect.true "1 in (1,1) is OK"
        , test "Invalid on invalid board" <|
            \_ ->
                valid (empty |> insert ( 1, 1 ) 1 |> insert ( 2, 1 ) 1) ( 1, 1 )
                    |> Expect.false "1 in (1,1) is not OK"
        ]
