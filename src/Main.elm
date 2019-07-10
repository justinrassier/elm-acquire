module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Model =
    { board : List Int
    , selectedSquare : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { board = List.range 0 143, selectedSquare = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = SquareClicked Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SquareClicked squareNumber ->
            ( { model | selectedSquare = Just squareNumber }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "board" ] (viewBoardSquares model.board)


viewBoardSquares : List Int -> List (Html Msg)
viewBoardSquares board =
    let
        cols =
            round (sqrt (toFloat (List.length board)))

        rows =
            round (sqrt (toFloat (List.length board)))
    in
    board
        |> List.map
            (\squareNumber ->
                div
                    [ class "flex flex-col justify-center h-24 border border-black border-solid text-center hover:bg-gray-400"
                    , onClick (SquareClicked squareNumber)
                    ]
                    [ text (boardNumberToSquareNumber cols squareNumber ++ boardNumberToSquareLetter cols squareNumber)
                    ]
            )


boardNumberToSquareNumber : Int -> Int -> String
boardNumberToSquareNumber cols value =
    String.fromInt (modBy cols value + 1)


boardNumberToSquareLetter : Int -> Int -> String
boardNumberToSquareLetter cols value =
    String.fromChar (Char.fromCode ((value // cols) + 65))



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
