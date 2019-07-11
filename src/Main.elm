module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Model =
    { selectedSquare : Maybe Int
    , board : List BoardSquare
    }


type alias BoardSquare =
    { id : Int
    , state : SquareState
    }


type SquareState
    = Empty
    | Tiled
    | Hotelled


init : ( Model, Cmd Msg )
init =
    ( { board = initBoard, selectedSquare = Nothing }, Cmd.none )


initBoard : List BoardSquare
initBoard =
    List.range 0 143
        |> List.map (\v -> { id = v, state = Empty })



---- UPDATE ----


type Msg
    = SquareClicked Int
    | LayTile Int
    | CancelTile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SquareClicked squareId ->
            ( { model | selectedSquare = Just squareId }, Cmd.none )

        LayTile squareId ->
            Debug.todo "update square with 'Tiled' state" ( model, Cmd.none )

        CancelTile ->
            ( { model | selectedSquare = Nothing }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "board" ] (viewBoardSquares model.board model.selectedSquare)


viewBoardSquares : List BoardSquare -> Maybe Int -> List (Html Msg)
viewBoardSquares board selectedSquare =
    let
        cols : Int
        cols =
            round (sqrt (toFloat (List.length board)))

        rows : Int
        rows =
            round (sqrt (toFloat (List.length board)))
    in
    board
        |> List.map
            (\square ->
                let
                    baseClassList =
                        "cursor-pointer flex flex-col justify-center h-24 border border-black border-solid text-center hover:bg-gray-400"

                    isSelected : Bool
                    isSelected =
                        selectedSquare
                            |> Maybe.map (\selectedSquareNumber -> selectedSquareNumber == square.id)
                            |> Maybe.withDefault False
                in
                if isSelected then
                    div
                        [ class (baseClassList ++ " bg-blue-700 text-white shadow-xl hover:bg-blue-700")
                        ]
                        [ span []
                            [ button [ onClick (LayTile square.id) ] [ text "lay" ]
                            , button [ onClick CancelTile ] [ text "cancel" ]
                            ]
                        ]

                else
                    div
                        [ class baseClassList
                        , onClick (SquareClicked square.id)
                        ]
                        [ text (boardNumberToSquareNumber cols square.id ++ boardNumberToSquareLetter cols square.id)
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
