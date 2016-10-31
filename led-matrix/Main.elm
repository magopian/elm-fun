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


loadSame : Int -> Int -> String -> String
loadSame width height value =
    value
        |> String.repeat width
        |> (++) "\n"
        |> String.repeat height
        |> String.trim


loadEmpty : Int -> Int -> String
loadEmpty width height =
    loadSame width height "0"


loadFull : Int -> Int -> String
loadFull width height =
    loadSame width height "1"



-- Model


type Msg
    = ToggleLed RowIndex ColIndex
    | UpdateMatrix String
    | LoadA
    | LoadSmiley
    | LoadEmpty
    | LoadFull
    | ChangeMatrixWidth String
    | ChangeMatrixHeight String
    | ChangeEnforceSquare Bool


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
    , matrixWidth : Int
    , matrixHeight : Int
    , enforceSquare : Bool
    }


emptyRow : Int -> Array.Array Bool
emptyRow size =
    Array.repeat size False


initialModel : Model
initialModel =
    { matrix = textToMatrix (loadEmpty 8 8), matrixWidth = 8, matrixHeight = 8, enforceSquare = True }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleLed row col ->
            let
                prevLedStatus =
                    getLedStatus row col model
            in
                { model
                    | matrix =
                        model
                            |> setLedStatus row col (not prevLedStatus)
                }

        UpdateMatrix text ->
            let
                lines =
                    String.split "\n" text

                max_width =
                    lines
                        |> List.map String.length
                        |> List.foldr max 0

                newMatrix =
                    textToMatrix text
            in
                { model
                    | matrix = newMatrix
                    , matrixWidth = max_width
                    , matrixHeight = Array.length newMatrix
                }

        LoadA ->
            let
                newMatrix =
                    textToMatrix loadA

                size =
                    Array.length newMatrix
            in
                { model
                    | matrix = newMatrix
                    , matrixWidth = size
                    , matrixHeight = size
                }

        LoadSmiley ->
            let
                newMatrix =
                    textToMatrix loadSmiley

                size =
                    Array.length newMatrix
            in
                { model
                    | matrix = newMatrix
                    , matrixWidth = size
                    , matrixHeight = size
                }

        LoadEmpty ->
            { model
                | matrix = textToMatrix (loadEmpty model.matrixWidth model.matrixHeight)
            }

        LoadFull ->
            { model | matrix = textToMatrix (loadFull model.matrixWidth model.matrixHeight) }

        ChangeMatrixWidth newWidth ->
            case String.toInt newWidth of
                Err msg ->
                    model

                Ok width ->
                    let
                        height =
                            if model.enforceSquare then
                                width
                            else
                                model.matrixHeight
                    in
                        { model
                            | matrix = textToMatrix (loadEmpty width height)
                            , matrixWidth = width
                            , matrixHeight = height
                        }

        ChangeMatrixHeight newHeight ->
            case String.toInt newHeight of
                Err msg ->
                    model

                Ok height ->
                    let
                        width =
                            if model.enforceSquare then
                                height
                            else
                                model.matrixWidth
                    in
                        { model
                            | matrix = textToMatrix (loadEmpty width height)
                            , matrixWidth = width
                            , matrixHeight = height
                        }

        ChangeEnforceSquare checked ->
            { model | enforceSquare = checked }


getLedStatus : RowIndex -> ColIndex -> Model -> Bool
getLedStatus row col model =
    model.matrix
        |> Array.get row
        |> Maybe.withDefault (emptyRow model.matrixWidth)
        |> Array.get col
        |> Maybe.withDefault False


setLedStatus : RowIndex -> ColIndex -> Bool -> Model -> Matrix
setLedStatus rowIndex colIndex status model =
    let
        row =
            model.matrix
                |> Array.get rowIndex
                |> Maybe.withDefault (emptyRow model.matrixWidth)

        updatedRow =
            row
                |> Array.set colIndex status
    in
        model.matrix
            |> Array.set rowIndex updatedRow



-- View


view : Model -> Html.Html Msg
view model =
    Html.div
        []
        [ Html.div
            []
            [ Html.text
                "Matrix stays a square: "
            , Html.input
                [ Html.Attributes.type' "checkbox"
                , Html.Attributes.checked model.enforceSquare
                , Html.Events.onCheck ChangeEnforceSquare
                ]
                []
            ]
        , Html.div
            []
            [ Html.text
                "Matrix width: "
            , Html.input
                [ Html.Attributes.type' "range"
                , Html.Attributes.value <| toString model.matrixWidth
                , Html.Attributes.min "1"
                , Html.Attributes.max "32"
                , Html.Events.onInput ChangeMatrixWidth
                ]
                []
            , Html.input
                [ Html.Attributes.value <|
                    toString model.matrixWidth
                , Html.Events.onInput ChangeMatrixWidth
                ]
                []
            ]
        , Html.div
            []
            [ Html.text
                "Matrix height: "
            , Html.input
                [ Html.Attributes.type' "range"
                , Html.Attributes.value <| toString model.matrixHeight
                , Html.Attributes.min "1"
                , Html.Attributes.max "32"
                , Html.Events.onInput ChangeMatrixHeight
                ]
                []
            , Html.input
                [ Html.Attributes.value <|
                    toString model.matrixHeight
                , Html.Events.onInput ChangeMatrixHeight
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
