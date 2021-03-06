module Board exposing (Board, Position, Value, box, boxElements, boxIndex, column, empty, generate, generator, get, insert, options, positions, puzzle, remove, row, solver, valid)

import Dict exposing (Dict)
import Random exposing (Generator, Seed)
import Random.List
import Set exposing (Set)


type alias Position =
    ( Int, Int )


{-| A value in a position
-}
type alias Value =
    Maybe Int


type alias Box =
    Int


type alias Location =
    { row : Int, column : Int, box : Box }


type alias Board =
    Dict Position Int

get : Position -> Board -> Value
get =
    Dict.get


insert : Position -> Int -> Board -> Board
insert =
    Dict.insert


remove : Position -> Board -> Board
remove =
    Dict.remove


positions : List Position
positions =
    List.range 0 80
        |> List.map (\i -> ( (i // 9) + 1, modBy 9 i + 1 ))


firstEmpty : Board -> Maybe Position
firstEmpty b =
    positions |> List.filter (\p -> get p b == Nothing) |> List.head


rowElements : Int -> List Position
rowElements i =
    List.range 1 9 |> List.map (\e -> ( i, e ))


row : Board -> Int -> List Value
row b i =
    List.range 1 9
        |> List.map (\e -> Dict.get ( i, e ) b)


columnElements : Int -> List Position
columnElements i =
    List.range 1 9 |> List.map (\e -> ( e, i ))


column : Board -> Int -> List Value
column b i =
    List.range 1 9
        |> List.map (\e -> Dict.get ( e, i ) b)


boxElements : Int -> List Position
boxElements index =
    let
        ( xRange, yRange ) =
            case index of
                1 ->
                    ( List.range 1 3, List.range 1 3 )

                2 ->
                    ( List.range 4 6, List.range 1 3 )

                3 ->
                    ( List.range 7 9, List.range 1 3 )

                4 ->
                    ( List.range 1 3, List.range 4 6 )

                5 ->
                    ( List.range 4 6, List.range 4 6 )

                6 ->
                    ( List.range 7 9, List.range 4 6 )

                7 ->
                    ( List.range 1 3, List.range 7 9 )

                8 ->
                    ( List.range 4 6, List.range 7 9 )

                9 ->
                    ( List.range 7 9, List.range 7 9 )

                _ ->
                    ( [], [] )
    in
    List.concatMap (\x -> List.map (\y -> ( x, y )) yRange) xRange


box : Board -> Int -> List Value
box b i =
    boxElements i
        |> List.map (\p -> get p b)


boxIndex : Position -> Box
boxIndex ( x, y ) =
    1 + 3 * ((y - 1) // 3) + ((x - 1) // 3)


getLocation : Position -> Location
getLocation pos =
    { row = Tuple.first pos, column = Tuple.second pos, box = boxIndex pos }


{-| Get all valid options to fill a position with considering only basic rules
The value a position already has is considered an option
-}
options : Board -> Position -> Set Int
options board position =
    let
        location =
            getLocation position

        filled =
            [ rowElements location.row
            , columnElements location.column
            , boxElements location.box
            ]
                |> List.concatMap identity
                |> List.filter ((/=) position)
                |> List.filterMap (\p -> get p board)
                |> Set.fromList
    in
    Set.diff (Set.fromList (List.range 1 9)) filled


{-| True if the value of the position has an obvious problem,
considering the basic rules of Sudoku
-}
valid : Board -> Position -> Bool
valid board position =
    case get position board of
        Nothing ->
            True

        Just v ->
            Set.member v (options board position)


puzzle : Generator (Maybe Board)
puzzle =
    let
        toRemove =
            Random.int 15 30
    in
    generatePuzzle toRemove generator


generatePuzzle : Generator Int -> Generator (Maybe Board) -> Generator (Maybe Board)
generatePuzzle i board =
    board
        |> Random.map (Maybe.withDefault Dict.empty >> Dict.toList)
        |> Random.map2 popElements i
        |> Random.andThen identity
        |> Random.map (Dict.fromList >> Just)


popElements : Int -> List ( Position, Int ) -> Generator (List ( Position, Int ))
popElements i list =
    if i > 0 then
        Random.List.shuffle list
            |> Random.map (List.tail >> Maybe.withDefault [])
            |> Random.andThen (popElements (i - 1))

    else
        Random.constant list


{-| Generate a filled board
-}
generator : Generator (Maybe Board)
generator =
    Random.int Random.minInt Random.maxInt
        |> Random.map (Random.initialSeed >> generate)


generate : Seed -> Maybe Board
generate seed =
    solve empty seed


solver : Board -> Generator (Maybe Board)
solver board =
    Random.int Random.minInt Random.maxInt
        |> Random.map (Random.initialSeed >> solve board)


solve : Board -> Seed -> Maybe Board
solve board seed =
    let
        ( boards, _ ) =
            prependOptions ( [ board ], seed )
    in
    List.head boards


prependOptions : ( List Board, Seed ) -> ( List Board, Seed )
prependOptions ( boards, seed ) =
    case boards of
        [] ->
            ( [], seed )

        b :: bs ->
            case firstEmpty b of
                Nothing ->
                    ( boards, seed )

                Just p ->
                    let
                        ( newBoards, newSeed ) =
                            naiveFill p ( b, seed )
                    in
                    case newBoards of
                        -- Nothing found? Backtrack!
                        [] ->
                            prependOptions (prependOptions ( bs, newSeed ))

                        _ ->
                            prependOptions ( List.append newBoards bs, newSeed )


{-| Add an element to a board, in attempt to solve the board
-}
naiveFill : Position -> ( Board, Seed ) -> ( List Board, Seed )
naiveFill position ( board, seed ) =
    let
        optionsGen =
            options board position |> Set.toList |> Random.List.shuffle

        ( opts, nextSeed ) =
            Random.step optionsGen seed
    in
    ( List.map (\val -> insert position val board) opts, nextSeed )


empty : Board
empty =
    Dict.empty
