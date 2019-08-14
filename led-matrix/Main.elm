module Main exposing (ColIndex, LedStatus, Matrix, Model, Msg(..), QueryData, Row, RowIndex, displayLed, displayMatrix, displayRow, emptyRow, getLedStatus, init, initialModel, intParamWithDefault, loadA, loadEmpty, loadFull, loadSame, loadSmiley, main, matrixToText, parseLines, parseUrl, queryParser, setLedStatus, stringToBool, textToMatrix, update, urlFromData, view)

import Array
import Browser
import Browser.Navigation as Nav
import Html
import Html.Attributes
import Html.Events
import Http
import String
import Url
import Url.Parser as UrlParser exposing ((<?>))
import Url.Parser.Query as Query



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
    | UrlChange Url.Url
    | UrlRequest Browser.UrlRequest


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
    , key : Nav.Key
    }


emptyRow : Int -> Array.Array Bool
emptyRow size =
    Array.repeat size False


initialModel : Nav.Key -> Model
initialModel key =
    { matrix = textToMatrix (loadEmpty 8 8)
    , matrixWidth = 8
    , matrixHeight = 8
    , enforceSquare = True
    , key = key
    }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLed row col ->
            let
                prevLedStatus =
                    getLedStatus row col model

                newMatrix =
                    model |> setLedStatus row col (not prevLedStatus)
            in
            ( model, Nav.pushUrl model.key (urlFromData model.matrixWidth model.matrixHeight (matrixToText newMatrix)) )

        UpdateMatrix text ->
            let
                lines =
                    String.split "\n" text

                maxWidth =
                    lines
                        |> List.map String.length
                        |> List.foldr max 0

                newMatrix =
                    textToMatrix text
            in
            ( model, Nav.pushUrl model.key (urlFromData maxWidth (Array.length newMatrix) text) )

        LoadA ->
            let
                newMatrix =
                    textToMatrix loadA

                size =
                    Array.length newMatrix
            in
            ( model, Nav.pushUrl model.key (urlFromData size size loadA) )

        LoadSmiley ->
            let
                newMatrix =
                    textToMatrix loadSmiley

                size =
                    Array.length newMatrix
            in
            ( model, Nav.pushUrl model.key (urlFromData size size loadSmiley) )

        LoadEmpty ->
            ( model, Nav.pushUrl model.key (urlFromData model.matrixWidth model.matrixHeight (loadEmpty model.matrixWidth model.matrixHeight)) )

        LoadFull ->
            ( model, Nav.pushUrl model.key (urlFromData model.matrixWidth model.matrixHeight (loadFull model.matrixWidth model.matrixHeight)) )

        ChangeMatrixWidth newWidth ->
            case String.toInt newWidth of
                Nothing ->
                    ( model, Cmd.none )

                Just width ->
                    let
                        height =
                            if model.enforceSquare then
                                width

                            else
                                model.matrixHeight
                    in
                    ( model, Nav.pushUrl model.key (urlFromData width height (loadEmpty width height)) )

        ChangeMatrixHeight newHeight ->
            case String.toInt newHeight of
                Nothing ->
                    ( model, Cmd.none )

                Just height ->
                    let
                        width =
                            if model.enforceSquare then
                                height

                            else
                                model.matrixWidth
                    in
                    ( model, Nav.pushUrl model.key (urlFromData width height (loadEmpty width height)) )

        ChangeEnforceSquare checked ->
            ( { model | enforceSquare = checked }, Cmd.none )

        UrlChange location ->
            let
                ( width, height, data ) =
                    parseUrl location
            in
            ( { model
                | matrixWidth = width
                , matrixHeight = height
                , matrix = textToMatrix data
              }
            , Cmd.none
            )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )


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


view : Model -> Browser.Document Msg
view model =
    { title = "Led Matrix"
    , body =
        [ Html.div
            []
            [ Html.div
                []
                [ Html.text
                    "Matrix stays a square: "
                , Html.input
                    [ Html.Attributes.type_ "checkbox"
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
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.value <| String.fromInt model.matrixWidth
                    , Html.Attributes.min "1"
                    , Html.Attributes.max "32"
                    , Html.Events.onInput ChangeMatrixWidth
                    ]
                    []
                , Html.input
                    [ Html.Attributes.value <|
                        String.fromInt model.matrixWidth
                    , Html.Events.onInput ChangeMatrixWidth
                    ]
                    []
                ]
            , Html.div
                []
                [ Html.text
                    "Matrix height: "
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.value <| String.fromInt model.matrixHeight
                    , Html.Attributes.min "1"
                    , Html.Attributes.max "32"
                    , Html.Events.onInput ChangeMatrixHeight
                    ]
                    []
                , Html.input
                    [ Html.Attributes.value <|
                        String.fromInt model.matrixHeight
                    , Html.Events.onInput ChangeMatrixHeight
                    ]
                    []
                ]
            , displayMatrix model.matrix
            , Html.textarea
                [ Html.Attributes.style "width" "500px"
                , Html.Attributes.style "height" "300px"
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
        ]
    }


displayLed : ColIndex -> RowIndex -> LedStatus -> Html.Html Msg
displayLed rowNum colNum led =
    Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "background-color"
            (if led then
                "lime"

             else
                "darkslategray"
            )
        , Html.Attributes.style "width" "1em"
        , Html.Attributes.style "height" "1em"
        , Html.Attributes.style "line-height" "1em"
        , Html.Attributes.style "border-radius" "0.5em"
        , Html.Attributes.style "margin" "0.2em"
        , Html.Events.onClick (ToggleLed rowNum colNum)
        ]
        []


displayRow : RowIndex -> Row -> Html.Html Msg
displayRow rowNum row =
    Html.div
        [ Html.Attributes.style "background-color" "black"
        , Html.Attributes.style "height" "1.4em"
        ]
        (Array.indexedMap (displayLed rowNum) row |> Array.toList)


displayMatrix : Matrix -> Html.Html Msg
displayMatrix matrix =
    Html.div
        [ Html.Attributes.style "float" "left" ]
        (Array.indexedMap displayRow matrix |> Array.toList)


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


type alias QueryData =
    { height : Int
    , width : Int
    , data : Maybe String
    }


queryParser : UrlParser.Parser (Int -> Int -> Maybe String -> a) a
queryParser =
    UrlParser.top
        <?> intParamWithDefault "height" 8
        <?> intParamWithDefault "width" 8
        <?> Query.string "data"


intParamWithDefault : String -> Int -> Query.Parser Int
intParamWithDefault param default =
    Query.custom param
        (\list ->
            case list of
                [] ->
                    default

                val :: _ ->
                    val
                        |> String.toInt
                        |> Maybe.withDefault default
        )


parseLines : Maybe String -> Int -> Int -> String
parseLines data height width =
    data
        |> Maybe.withDefault (loadEmpty height width)


parseUrl : Url.Url -> ( Int, Int, String )
parseUrl location =
    let
        { width, height, data } =
            UrlParser.parse (UrlParser.map QueryData queryParser) location
                |> Maybe.withDefault { width = 8, height = 8, data = Nothing }
    in
    ( width, height, parseLines data width height )


urlFromData : Int -> Int -> String -> String
urlFromData width height data =
    "?width="
        ++ String.fromInt width
        ++ "&height="
        ++ String.fromInt height
        ++ "&data="
        ++ Url.percentEncode data


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( width, height, data ) =
            parseUrl url
    in
    ( initialModel key, Nav.pushUrl key (urlFromData width height data) )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }
