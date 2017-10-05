module Main exposing (..)

import Collage
import Element
import Examples exposing (house, star, elm)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import List
import Navigation
import String
import Translation as T
import UrlParser exposing ((<?>))


-- Types


type Msg
    = CommandsChange String
    | LoadHouse
    | LoadStar
    | LoadElm
    | DrawTurtle Bool
    | SetLanguage T.Language
    | UrlChange Navigation.Location


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


turtle : List String
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



-- Parsing URL


type alias QueryData =
    { lang : String
    , commands : Maybe String
    }


queryParser : UrlParser.Parser (String -> Maybe String -> a) a
queryParser =
    UrlParser.top <?> stringParamWithDefault "lang" "en" <?> UrlParser.stringParam "commands"


stringParamWithDefault : String -> String -> UrlParser.QueryParser (String -> a) a
stringParamWithDefault param default =
    UrlParser.customParam param (Maybe.withDefault default)


parseCommands : Maybe String -> T.Language -> List String
parseCommands commands language =
    case commands of
        Just commandList ->
            String.split "\n" commandList

        Nothing ->
            house language


parseUrl : Navigation.Location -> ( T.Language, List String )
parseUrl location =
    let
        { lang, commands } =
            UrlParser.parseHash (UrlParser.map QueryData queryParser) location
                |> Maybe.withDefault { lang = "en", commands = Nothing }

        language =
            T.codeToLanguage lang
    in
        ( language, parseCommands commands language )


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
        "?commands="
            ++ (commands
                    |> String.join "\n"
                    |> Http.encodeUri
               )
            ++ "&lang="
            ++ langStr



-- Update


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( language, commands ) =
            parseUrl location
    in
        Model commands True language ! [ Navigation.newUrl (urlFromCommands commands language) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CommandsChange commands ->
            model ! [ Navigation.newUrl (urlFromCommands (String.split "\n" commands) model.lang) ]

        LoadHouse ->
            model ! [ Navigation.newUrl (urlFromCommands (house model.lang) model.lang) ]

        LoadStar ->
            model ! [ Navigation.newUrl (urlFromCommands (star model.lang) model.lang) ]

        LoadElm ->
            model ! [ Navigation.newUrl (urlFromCommands (elm model.lang) model.lang) ]

        DrawTurtle bool ->
            { model | drawTurtle = bool } ! []

        SetLanguage lang ->
            model ! [ Navigation.newUrl (urlFromCommands model.commands lang) ]

        UrlChange newLocation ->
            let
                ( language, commands ) =
                    parseUrl newLocation
            in
                { model | lang = language, commands = commands } ! []



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
                        [ Html.Attributes.type_ "checkbox"
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
        isCurrent lang_ =
            lang == lang_

        button_ lang_ name =
            Html.button
                [ Html.Attributes.disabled (isCurrent lang_)
                , Html.Events.onClick (SetLanguage lang_)
                ]
                [ Html.text name ]
    in
        Html.div
            []
            [ button_ T.English "English"
            , button_ T.French "Français"
            ]


translateError : T.Language -> ErrorMessage -> String
translateError lang { line, error } =
    case error of
        ParseCommandError str ->
            T.translate lang (T.ParseCommandError line str)

        ParseFloatError str ->
            T.translate lang (T.ParseFloatError line str)



-- Main


main : Program Never Model Msg
main =
    Navigation.program UrlChange
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
            |> Result.mapError errorMessage


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
        splitMovesFromErrors_ :
            List ErrorMessage
            -> List Step
            -> List ( Int, Result Error Step )
            -> ( List ErrorMessage, List Step )
        splitMovesFromErrors_ errors moves movesAndErrors =
            case movesAndErrors of
                [] ->
                    ( List.reverse errors, List.reverse moves )

                head :: tail ->
                    case head of
                        ( _, Ok move ) ->
                            splitMovesFromErrors_
                                errors
                                (move :: moves)
                                tail

                        ( i, Err error ) ->
                            splitMovesFromErrors_
                                ((ErrorMessage (i + 1) error) :: errors)
                                moves
                                tail
    in
        splitMovesFromErrors_ [] [] movesAndErrors


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
        movesToPaths_ :
            Point
            -> Angle
            -> Bool
            -> List Collage.Path
            -> List Step
            -> List Collage.Path
        movesToPaths_ point angle draw paths moves =
            case moves of
                head :: tail ->
                    let
                        ( newPoint, newAngle, newDraw, path ) =
                            toPath point angle draw head
                    in
                        case path of
                            Nothing ->
                                movesToPaths_ newPoint newAngle newDraw paths tail

                            Just segment ->
                                movesToPaths_
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
        movesToPaths_ ( 0, 0 ) 90 True [] moves
