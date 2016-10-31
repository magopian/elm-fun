module Main exposing (..)

import Array
import Html
import Html.App
import Html.Attributes
import Html.Events
import String


-- Data


loadA : String
loadA =
    String.trim """
00011000
00100100
01000010
01000010
01111110
01000010
01000010
01000010
"""


loadSmiley : String
loadSmiley =
    String.trim """
00000000
00000000
00100100
00000000
00000000
01000010
00111100
00000000
"""


loadEmpty : Int -> String
loadEmpty size =
    "0"
        |> String.repeat size
        -- A row of <size> columns
        |>
            (++) "\n"
        |> String.repeat size
        -- <size> rows
        |>
            String.trim


loadFull : Int -> String
loadFull size =
    "1"
        |> String.repeat size
        -- A row of <size> columns
        |>
            (++) "\n"
        |> String.repeat size
        -- <size> rows
        |>
            String.trim



-- Model


type Msg
    = ToggleLed RowIndex ColIndex
    | UpdateMatrix String
    | LoadA
    | LoadSmiley
    | LoadEmpty
    | LoadFull
    | ChangeMatrixSize String


type alias RowIndex =
    Int


type alias ColIndex =
    Int


type alias LedStatus =
    Bool


type alias Row =
    Array.Array LedStatus


type alias Matrix =
    Array.Array Row


type alias Model =
    { matrix : Matrix
    , matrixSize : Int
    }


emptyRow : Int -> Array.Array Bool
emptyRow size =
    Array.repeat size False


initialModel : Model
initialModel =
    { matrix = textToMatrix (loadEmpty 8), matrixSize = 8 }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleLed row col ->
            let
                prevLedStatus =
                    getLedStatus row col model.matrix
            in
                { model
                    | matrix =
                        model.matrix
                            |> setLedStatus row col (not prevLedStatus)
                }

        UpdateMatrix text ->
            let
                newMatrix =
                    textToMatrix text
            in
                { model
                    | matrix = newMatrix
                    , matrixSize = Array.length newMatrix
                }

        LoadA ->
            let
                newMatrix =
                    textToMatrix loadA
            in
                { model
                    | matrix = newMatrix
                    , matrixSize = Array.length newMatrix
                }

        LoadSmiley ->
            let
                newMatrix =
                    textToMatrix loadSmiley
            in
                { model
                    | matrix = newMatrix
                    , matrixSize = Array.length newMatrix
                }

        LoadEmpty ->
            { model
                | matrix = textToMatrix (loadEmpty model.matrixSize)
            }

        LoadFull ->
            { model | matrix = textToMatrix (loadFull model.matrixSize) }

        ChangeMatrixSize newSize ->
            case String.toInt newSize of
                Err msg ->
                    model

                Ok size ->
                    { model
                        | matrix = textToMatrix (loadEmpty size)
                        , matrixSize = size
                    }


getLedStatus : RowIndex -> ColIndex -> Matrix -> Bool
getLedStatus row col matrix =
    matrix
        |> Array.get row
        |> Maybe.withDefault (emptyRow (Array.length matrix))
        |> Array.get col
        |> Maybe.withDefault False


setLedStatus : RowIndex -> ColIndex -> Bool -> Matrix -> Matrix
setLedStatus rowIndex colIndex status matrix =
    let
        row =
            matrix
                |> Array.get rowIndex
                |> Maybe.withDefault (emptyRow (Array.length matrix))

        updatedRow =
            row
                |> Array.set colIndex status
    in
        matrix
            |> Array.set rowIndex updatedRow



-- View


view : Model -> Html.Html Msg
view model =
    Html.div
        []
        [ Html.div
            []
            [ Html.text
                "Matrix size: "
            , Html.input
                [ Html.Attributes.type' "range"
                , Html.Attributes.value <| toString model.matrixSize
                , Html.Attributes.min "1"
                , Html.Attributes.max "32"
                , Html.Events.onInput ChangeMatrixSize
                ]
                []
            , Html.input
                [ Html.Attributes.value <|
                    toString model.matrixSize
                , Html.Events.onInput ChangeMatrixSize
                ]
                []
            ]
        , displayMatrix model.matrix
        , Html.textarea
            [ Html.Attributes.style
                [ ( "width", "500px" )
                , ( "height", "300px" )
                ]
            , Html.Attributes.value (matrixToText model.matrix)
            , Html.Events.onInput UpdateMatrix
            ]
            []
        , Html.div
            []
            [ Html.button
                [ Html.Events.onClick LoadA ]
                [ Html.text "A" ]
            , Html.button
                [ Html.Events.onClick LoadSmiley ]
                [ Html.text ":)" ]
            , Html.button
                [ Html.Events.onClick LoadEmpty ]
                [ Html.text "Empty" ]
            , Html.button
                [ Html.Events.onClick LoadFull ]
                [ Html.text "Full" ]
            ]
        ]


displayLed : ColIndex -> RowIndex -> LedStatus -> Html.Html Msg
displayLed rowNum colNum led =
    Html.div
        [ Html.Attributes.style
            [ ( "display", "inline-block" )
            , ( "background-color"
              , if led then
                    "lime"
                else
                    "darkslategray"
              )
            , ( "width", "1em" )
            , ( "height", "1em" )
            , ( "line-height", "1em" )
            , ( "border-radius", "0.5em" )
            , ( "margin", "0.2em" )
            ]
        , Html.Events.onClick (ToggleLed rowNum colNum)
        ]
        []


displayRow : RowIndex -> Row -> Html.Html Msg
displayRow rowNum row =
    Html.div
        [ Html.Attributes.style
            [ ( "background-color", "black" )
            , ( "height", "1.4em" )
            ]
        ]
        ((Array.indexedMap (displayLed rowNum) row) |> Array.toList)


displayMatrix : Matrix -> Html.Html Msg
displayMatrix matrix =
    Html.div
        [ Html.Attributes.style [ ( "float", "left" ) ] ]
        ((Array.indexedMap displayRow matrix) |> Array.toList)


matrixToText : Matrix -> String
matrixToText matrix =
    let
        rowToText : Row -> String
        rowToText row =
            row
                |> Array.toList
                |> List.map
                    (\b ->
                        if b then
                            "1"
                        else
                            "0"
                    )
                |> String.join ""
    in
        matrix
            |> Array.map rowToText
            |> Array.toList
            |> String.join "\n"


stringToBool : String -> Bool
stringToBool string =
    case string of
        "1" ->
            True

        _ ->
            False


textToMatrix : String -> Matrix
textToMatrix text =
    let
        rows =
            String.lines text
                |> Array.fromList

        lineToBools row =
            row
                |> String.toList
                |> List.map String.fromChar
                |> Array.fromList
                |> Array.map stringToBool
    in
        rows
            |> Array.map lineToBools



-- Main


main : Program Never
main =
    Html.App.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
