module Board exposing (Board, Position, box, column, emptyBoard, getBox, insert, options, positions, row, validBoard)

import Dict exposing (Dict)
import Random exposing (Generator)
import Set exposing (Set)


type alias Position =
    ( Int, Int )


type alias Box =
    Int


type alias Location =
    { row : Int, column : Int, box : Box }


type alias Board =
    Dict Position Int


type Msg
    = Number Int


get : Position -> Board -> Maybe Int
get =
    Dict.get


insert : Position -> Int -> Board -> Board
insert =
    Dict.insert


positions : List ( Int, Int )
positions =
    List.range 0 80
        |> List.map (\i -> ( (i // 9) + 1, modBy 9 i + 1 ))


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
        ( x, y ) =
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
    List.concatMap (\curX -> List.map (\curY -> ( curX, curY )) y) x
        |> List.map (\p -> get p b)


getBox : Position -> Box
getBox ( x, y ) =
    1 + 3 * ((y - 1) // 3) + ((x - 1) // 3)


getLocation : Position -> Location
getLocation pos =
    { row = Tuple.first pos, column = Tuple.second pos, box = getBox pos }


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


newNumber : Cmd Msg
newNumber =
    Random.generate Number (Random.int 1 9)


validBoard : Board
validBoard =
    List.foldl
        (\pos board -> insert pos (options board pos |> Set.toList |> List.head |> Maybe.withDefault 0) board)
        emptyBoard
        positions


emptyBoard : Board
emptyBoard =
    Dict.empty
