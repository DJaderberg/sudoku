module Main exposing (Msg(..), main, update, view)

import Board exposing (Board)
import Browser
import Browser.Events
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }



-- MODEL


type alias Highlight =
    Maybe Board.Position


type alias PositionValue =
    Maybe Int


type alias Model =
    { board : Board
    , highlight : Highlight
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.empty, highlight = Nothing }, generateBoardMsg )



-- UPDATE


type Msg
    = GenerateBoard
    | SetBoard Board
    | SetHighlight Board.Position
    | SetValue Board.Value
    | RemoveValue
    | SolveBoard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateBoard ->
            ( model
            , generateBoardMsg
            )

        SetBoard b ->
            ( { model | board = b }
            , Cmd.none
            )

        SetHighlight p ->
            ( { model | highlight = Just p }
            , Cmd.none
            )

        SetValue v ->
            ( { model | board = addValue model.highlight v model.board }
            , Cmd.none
            )

        RemoveValue ->
            ( { model | board = removeValue model.highlight model.board }
            , Cmd.none
            )

        SolveBoard ->
            ( model
            , Random.generate (Maybe.withDefault model.board >> SetBoard) (Board.solver model.board)
            )


addValue : Highlight -> Board.Value -> Board -> Board
addValue highlight value board =
    case ( value, highlight ) of
        ( Just i, Just position ) ->
            Board.insert position i board

        _ ->
            board


removeValue : Highlight -> Board -> Board
removeValue highlight board =
    case highlight of
        Nothing ->
            board

        Just position ->
            Board.remove position board


generateBoardMsg =
    Random.generate (Maybe.withDefault Board.empty >> SetBoard) Board.puzzle



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyUp keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toValue (Decode.field "key" Decode.string)


toValue : String -> Msg
toValue string =
    let
        deleteKeys =
            [ "Backspace", "Clear", "Cut", "Delete", "EraseEof" ]
    in
    if List.any ((==) string) deleteKeys then
        RemoveValue

    else
        SetValue (String.toInt string)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ viewBoard model.highlight model.board ]
        , button [ onClick GenerateBoard ] [ text "New board" ]
        , button [ onClick SolveBoard ] [ text "Solve" ]
        ]


viewBoard : Highlight -> Board -> Html Msg
viewBoard highlight board =
    List.range 1 9
        |> List.map (viewRow highlight board)
        |> Html.Styled.table [ css [ boardStyle ] ]


boardStyle : Style
boardStyle =
    batch
        [ borderCollapse collapse
        , border3 (px 4) solid (rgb 0 0 0)
        ]


viewRow : Highlight -> Board -> Int -> Html Msg
viewRow highlight board r =
    let
        rowPositions =
            List.range 1 9 |> List.map (\c -> ( r, c ))
    in
    Html.Styled.tr [] (rowPositions |> List.map (viewPosition highlight board))


viewPosition : Highlight -> Board -> Board.Position -> Html Msg
viewPosition highlight board position =
    position
        |> (\p -> Board.get p board)
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ""
        |> text
        |> List.singleton
        |> td [ css (positionStyle :: positionBorder position |> withHighlight highlight board position |> withError board position), onClick (SetHighlight position) ]


positionStyle : Style
positionStyle =
    batch
        [ positionFont
        , width (px 35)
        , height (px 35)
        , textAlign center
        , border3 (px 1) solid (rgb 0 0 0)
        ]


positionBorder : Board.Position -> List Style
positionBorder ( r, c ) =
    let
        thickBorder =
            px 3

        top =
            if modBy 3 (r - 1) == 0 then
                Just (borderTopWidth thickBorder)

            else
                Nothing

        right =
            if modBy 3 c == 0 then
                Just (borderRightWidth thickBorder)

            else
                Nothing
    in
    List.filterMap identity [ top, right ]


withHighlight : Highlight -> Board -> Board.Position -> List Style -> List Style
withHighlight highlight board position list =
    case highlight of
        Nothing ->
            list

        Just h ->
            List.append (getHighlight board h position) list


withError : Board -> Board.Position -> List Style -> List Style
withError board position list =
    if Board.valid board position then
        list

    else
        color (rgb 255 0 0) :: list


getHighlight : Board -> Board.Position -> Board.Position -> List Style
getHighlight board highlight position =
    let
        value =
            Board.get highlight board
    in
    if position == highlight then
        [ backgroundColor (rgb 101 215 235) ]

    else if value /= Nothing && value == Board.get position board then
        [ backgroundColor (rgb 131 165 185) ]

    else if Tuple.first position == Tuple.first highlight || Tuple.second position == Tuple.second highlight || Board.boxIndex position == Board.boxIndex highlight then
        [ backgroundColor (rgb 204 210 212) ]

    else
        []


positionFont : Style
positionFont =
    batch
        [ fontFamilies [ "sans-serif" ]
        , fontSize (px 32)
        , fontWeight normal
        ]
