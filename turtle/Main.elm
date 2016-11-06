module Main exposing (..)

import Base64
import Collage
import Element
import Examples exposing (house, star, elm)
import Html exposing (Html)
import Html.App
import Html.Attributes
import Html.Events
import List
import String
import Translation as T


-- Update


type Msg
    = CommandsChange String
    | LoadHouse
    | LoadStar
    | LoadElm
    | DrawTurtle Bool
    | SetLanguage T.Language


type Error
    = ParseCommandError String
    | ParseFloatError String


type alias ErrorMessage =
    { line : Int
    , error : Error
    }


type alias Model =
    { commands : List String
    , drawTurtle : Bool
    , lang : T.Language
    }


turtle =
    [ "Forward 10"
    , "Right 155"
    , "Forward 11.2"
    , "Right 115"
    , "Forward 9.4"
    , "Right 115"
    , "Forward 11.2"
    , "Left 25"
    ]


type alias Flags =
    { lang : Maybe String
    , hash : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init { lang, hash } =
    let
        langCode =
            Maybe.withDefault "en" lang

        language =
            case langCode of
                "fr" ->
                    T.French

                _ ->
                    T.English

        defaultModel =
            Model (house language) True language
    in
        case hash of
            Nothing ->
                defaultModel ! []

            Just hash ->
                case Base64.decode hash of
                    Ok commands ->
                        (Model (String.split "\n" commands) True language) ! []

                    Err msg ->
                        let
                            _ =
                                Debug.log "failed to decode hash" msg
                        in
                            defaultModel ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CommandsChange commands ->
            { model | commands = String.split "\n" commands } ! []

        LoadHouse ->
            { model | commands = (house model.lang) } ! []

        LoadStar ->
            { model | commands = (star model.lang) } ! []

        LoadElm ->
            { model | commands = (elm model.lang) } ! []

        DrawTurtle bool ->
            { model | drawTurtle = bool } ! []

        SetLanguage lang ->
            { model | lang = lang } ! []



-- View


view : Model -> Html Msg
view model =
    let
        commands =
            if model.drawTurtle then
                (model.commands ++ turtle)
            else
                model.commands

        parsed =
            commandsToMoves commands

        ( errors, moves ) =
            splitMovesFromErrors parsed
    in
        Html.div []
            [ languageSwitcher model.lang
            , Html.p
                []
                [ Html.text <| T.translate model.lang T.TurtleCommands ]
            , Html.p
                []
                [ Html.label
                    []
                    [ Html.input
                        [ Html.Attributes.type' "checkbox"
                        , Html.Attributes.checked model.drawTurtle
                        , Html.Events.onCheck DrawTurtle
                        ]
                        []
                    , Html.text <| T.translate model.lang T.DisplayTurtle
                    ]
                ]
            , Html.textarea
                [ Html.Attributes.style
                    [ ( "width", "600px" )
                    , ( "height", "600px" )
                    , ( "float", "left" )
                    ]
                , Html.Events.onInput CommandsChange
                , Html.Attributes.value <| String.join "\n" model.commands
                ]
                []
            , Element.toHtml <|
                Collage.collage 600 600 <|
                    ((drawShape <| Collage.rect 600 600)
                        :: List.map drawPath (movesToPaths moves)
                    )
            , Html.button
                [ Html.Events.onClick LoadHouse ]
                [ Html.text <| T.translate model.lang T.House ]
            , Html.button
                [ Html.Events.onClick LoadStar ]
                [ Html.text <| T.translate model.lang T.Star ]
            , Html.button
                [ Html.Events.onClick LoadElm ]
                [ Html.text <| T.translate model.lang T.Elm ]
            , Html.p
                []
                [ Html.a
                    [ Html.Attributes.href
                        (urlFromCommands model.commands model.lang)
                    ]
                    [ Html.text <| T.translate model.lang T.ShareUrl ]
                ]
            , Html.pre []
                [ errors
                    |> List.map (translateError model.lang)
                    |> String.join "\n"
                    |> Html.text
                ]
            ]


languageSwitcher : T.Language -> Html Msg
languageSwitcher lang =
    let
        -- Check if a language is the current language
        isCurrent lang' =
            lang == lang'

        button' lang' name =
            Html.button
                [ Html.Attributes.disabled (isCurrent lang')
                , Html.Events.onClick (SetLanguage lang')
                ]
                [ Html.text name ]
    in
        Html.div
            []
            [ button' T.English "English"
            , button' T.French "Français"
            ]


translateError : T.Language -> ErrorMessage -> String
translateError lang { line, error } =
    case error of
        ParseCommandError str ->
            T.translate lang (T.ParseCommandError line str)

        ParseFloatError str ->
            T.translate lang (T.ParseFloatError line str)



-- Main


main =
    Html.App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- Helpers


type alias Point =
    ( Float, Float )


type alias Angle =
    Float


type alias Segment =
    ( Point, Point )


type Step
    = Forward Float
    | Left Angle
    | Right Angle
    | PenUp
    | PenDown


drawShape : Collage.Shape -> Collage.Form
drawShape shape =
    shape |> Collage.outlined Collage.defaultLine


drawPath : Collage.Path -> Collage.Form
drawPath path =
    path |> Collage.traced Collage.defaultLine


stringToFloat : String -> Result Error Float
stringToFloat str =
    -- This function is needed to "map" the error to our own type, that we'll
    -- use with i18n. We also want to keep the faulty string for our final
    -- translated message.
    let
        errorMessage _ =
            ParseFloatError str
    in
        String.toFloat str
            |> Result.formatError errorMessage


parseCommand : String -> Result Error Step
parseCommand command =
    case String.split " " command of
        [ "Forward", distance ] ->
            Result.map Forward (stringToFloat distance)

        [ "Avance", distance ] ->
            Result.map Forward (stringToFloat distance)

        [ "Left", angle ] ->
            Result.map Left (stringToFloat angle)

        [ "Gauche", angle ] ->
            Result.map Left (stringToFloat angle)

        [ "Right", angle ] ->
            Result.map Right (stringToFloat angle)

        [ "Droite", angle ] ->
            Result.map Right (stringToFloat angle)

        [ "PenUp" ] ->
            Ok PenUp

        [ "LeveStylo" ] ->
            Ok PenUp

        [ "PenDown" ] ->
            Ok PenDown

        [ "BaisseStylo" ] ->
            Ok PenDown

        _ ->
            Err <| ParseCommandError command


commandsToMoves : List String -> List ( Int, Result Error Step )
commandsToMoves commands =
    List.indexedMap
        (\i command ->
            ( i, parseCommand command )
        )
        commands


splitMovesFromErrors :
    List ( Int, Result Error Step )
    -> ( List ErrorMessage, List Step )
splitMovesFromErrors movesAndErrors =
    let
        splitMovesFromErrors' :
            List ErrorMessage
            -> List Step
            -> List ( Int, Result Error Step )
            -> ( List ErrorMessage, List Step )
        splitMovesFromErrors' errors moves movesAndErrors =
            case movesAndErrors of
                [] ->
                    ( List.reverse errors, List.reverse moves )

                head :: tail ->
                    case head of
                        ( _, Ok move ) ->
                            splitMovesFromErrors'
                                errors
                                (move :: moves)
                                tail

                        ( i, Err error ) ->
                            splitMovesFromErrors'
                                ((ErrorMessage i error) :: errors)
                                moves
                                tail
    in
        splitMovesFromErrors' [] [] movesAndErrors


toPath :
    Point
    -> Angle
    -> Bool
    -> Step
    -> ( Point, Angle, Bool, Maybe Collage.Path )
toPath (( x, y ) as currentPoint) currentAngle draw step =
    case step of
        Forward amount ->
            let
                newPoint =
                    ( x + (cos (degrees currentAngle)) * amount
                    , y + (sin (degrees currentAngle)) * amount
                    )
            in
                ( newPoint
                , currentAngle
                , draw
                , Just <| Collage.segment currentPoint newPoint
                )

        Left angle ->
            ( currentPoint, currentAngle + angle, draw, Nothing )

        Right angle ->
            ( currentPoint, currentAngle - angle, draw, Nothing )

        PenUp ->
            ( currentPoint, currentAngle, False, Nothing )

        PenDown ->
            ( currentPoint, currentAngle, True, Nothing )


movesToPaths : List Step -> List Collage.Path
movesToPaths moves =
    let
        movesToPaths' :
            Point
            -> Angle
            -> Bool
            -> List Collage.Path
            -> List Step
            -> List Collage.Path
        movesToPaths' point angle draw paths moves =
            case moves of
                head :: tail ->
                    let
                        ( newPoint, newAngle, newDraw, path ) =
                            toPath point angle draw head
                    in
                        case path of
                            Nothing ->
                                movesToPaths' newPoint newAngle newDraw paths tail

                            Just segment ->
                                movesToPaths'
                                    newPoint
                                    newAngle
                                    newDraw
                                    (if newDraw then
                                        (segment :: paths)
                                     else
                                        paths
                                    )
                                    tail

                [] ->
                    paths
    in
        movesToPaths' ( 0, 0 ) 90 True [] moves


urlFromCommands : List String -> T.Language -> String
urlFromCommands commands lang =
    let
        langStr =
            case lang of
                T.English ->
                    "en"

                T.French ->
                    "fr"
    in
        "?hash="
            ++ (commands
                    |> String.join "\n"
                    |> Base64.encode
                    |> Result.withDefault ""
               )
            ++ "&lang="
            ++ langStr
