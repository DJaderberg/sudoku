module Board exposing (Board, Position, box, boxIndex, column, empty, generate, generator, insert, options, positions, row)

import Dict exposing (Dict)
import Random exposing (Generator, Seed)
import Random.List
import Set exposing (Set)


type alias Position =
    ( Int, Int )


type alias Box =
    Int


type alias Location =
    { row : Int, column : Int, box : Box }


type alias Board =
    Dict Position Int


get : Position -> Board -> Maybe Int
get =
    Dict.get


insert : Position -> Int -> Board -> Board
insert =
    Dict.insert


positions : List Position
positions =
    List.range 0 80
        |> List.map (\i -> ( (i // 9) + 1, modBy 9 i + 1 ))


firstEmpty : Board -> Maybe Position
firstEmpty b =
    positions |> List.filter (\p -> get p b == Nothing) |> List.head


row : Board -> Int -> List (Maybe Int)
row b i =
    List.range 1 9
        |> List.map (\e -> Dict.get ( i, e ) b)


column : Board -> Int -> List (Maybe Int)
column b i =
    List.range 1 9
        |> List.map (\e -> Dict.get ( e, i ) b)


box : Board -> Int -> List (Maybe Int)
box b i =
    let
        ( xRange, yRange ) =
            case i of
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
        |> List.map (\p -> get p b)


boxIndex : Position -> Box
boxIndex ( x, y ) =
    1 + 3 * ((y - 1) // 3) + ((x - 1) // 3)


getLocation : Position -> Location
getLocation pos =
    { row = Tuple.first pos, column = Tuple.second pos, box = boxIndex pos }


{-| Get all valid options to fill a position with
considering only basic rules
-}
options : Board -> Position -> Set Int
options board position =
    let
        location =
            getLocation position

        filled =
            [ row board location.row, column board location.column, box board location.box ]
                |> List.concatMap identity
                |> List.filterMap identity
                |> Set.fromList
    in
    case get position board of
        Nothing ->
            Set.diff (Set.fromList (List.range 1 9)) filled

        Just x ->
            Set.singleton x


generator : Generator (Maybe Board)
generator =
    Random.int Random.minInt Random.maxInt
        |> Random.map (Random.initialSeed >> generate)


generate : Seed -> Maybe Board
generate seed =
    let
        ( boards, _ ) =
            prependOptions ( [ empty ], seed )
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
