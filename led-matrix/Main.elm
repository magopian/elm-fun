module Main exposing (..)

import Array
import Html
import Html.App
import Html.Attributes
import Html.Events
import String


-- Model


type Msg
    = ToggleLed RowIndex ColIndex


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
    }


initialModel : Model
initialModel =
    let
        matrix =
            Array.repeat 8 (Array.repeat 8 False)
    in
        { matrix = Array.set 3 (Array.repeat 8 True) matrix }



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


getLedStatus : RowIndex -> ColIndex -> Matrix -> Bool
getLedStatus row col matrix =
    matrix
        |> Array.get row
        |> Maybe.withDefault (Array.repeat 8 False)
        |> Array.get col
        |> Maybe.withDefault False


setLedStatus : RowIndex -> ColIndex -> Bool -> Matrix -> Matrix
setLedStatus rowIndex colIndex status matrix =
    let
        row =
            matrix
                |> Array.get rowIndex
                |> Maybe.withDefault (Array.repeat 8 False)

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
        [ displayMatrix model.matrix
        , Html.textarea
            [ Html.Attributes.style
                [ ( "width", "500px" )
                , ( "height", "300px" )
                ]
            , Html.Attributes.value (matrixToText model.matrix)
            ]
            []
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
                |> List.map toString
                |> String.join ","
    in
        matrix
            |> Array.map rowToText
            |> Array.toList
            |> String.join "\n"



-- Main


main =
    Html.App.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
