module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, src)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "board" ] (viewBoardSquares 4 4)


viewBoardSquares : Int -> Int -> List (Html Msg)
viewBoardSquares cols rows =
    List.range 0 ((cols * rows) - 1)
        |> List.map
            (\v ->
                div [ class "flex flex-col justify-center h-24 border border-black border-solid text-center" ]
                    [ text (boardNumberToSquareNumber cols v ++ boardNumberToSquareLetter cols v) ]
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
