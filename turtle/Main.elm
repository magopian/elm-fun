module Main exposing (Angle, Error(..), ErrorMessage, Model, Msg(..), Point, QueryData, Segment, Step(..), commandsToMoves, drawPath, drawShape, init, languageSwitcher, main, movesToPaths, parseCommand, parseCommands, parseUrl, queryParser, splitMovesFromErrors, stringParamWithDefault, stringToFloat, toPath, translateError, turtle, update, urlFromCommands, view)

import Browser
import Browser.Navigation as Nav
import Collage
import Collage.Render
import Examples exposing (elm, house, star)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List
import String
import Translation as T
import Url
import Url.Parser as UrlParser exposing ((<?>))
import Url.Parser.Query as Query



-- Types


type Msg
    = CommandsChange String
    | LoadHouse
    | LoadStar
    | LoadElm
    | DrawTurtle Bool
    | SetLanguage T.Language
    | UrlChange Url.Url
    | UrlRequest Browser.UrlRequest


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
    , key : Nav.Key
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
    UrlParser.top
        <?> stringParamWithDefault "lang" "en"
        <?> Query.string "commands"


stringParamWithDefault : String -> String -> Query.Parser String
stringParamWithDefault param default =
    Query.custom param (List.head >> Maybe.withDefault default)


parseCommands : Maybe String -> T.Language -> List String
parseCommands commands language =
    case commands of
        Just commandList ->
            String.split "\n" commandList

        Nothing ->
            house language


parseUrl : Url.Url -> ( T.Language, List String )
parseUrl location =
    let
        { lang, commands } =
            UrlParser.parse (UrlParser.map QueryData queryParser) location
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
                |> Url.percentEncode
           )
        ++ "&lang="
        ++ langStr



-- Update


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( language, commands ) =
            parseUrl url
    in
    ( Model commands True language key, Nav.pushUrl key (urlFromCommands commands language) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CommandsChange commands ->
            ( model, Nav.pushUrl model.key (urlFromCommands (String.split "\n" commands) model.lang) )

        LoadHouse ->
            ( model, Nav.pushUrl model.key (urlFromCommands (house model.lang) model.lang) )

        LoadStar ->
            ( model, Nav.pushUrl model.key (urlFromCommands (star model.lang) model.lang) )

        LoadElm ->
            ( model, Nav.pushUrl model.key (urlFromCommands (elm model.lang) model.lang) )

        DrawTurtle bool ->
            ( { model | drawTurtle = bool }, Cmd.none )

        SetLanguage lang ->
            ( model, Nav.pushUrl model.key (urlFromCommands model.commands lang) )

        UrlChange newLocation ->
            let
                ( language, commands ) =
                    parseUrl newLocation
            in
            ( { model | lang = language, commands = commands }, Cmd.none )

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



-- View


view : Model -> Browser.Document Msg
view model =
    let
        commands =
            if model.drawTurtle then
                model.commands ++ turtle

            else
                model.commands

        parsed =
            commandsToMoves commands

        ( errors, moves ) =
            splitMovesFromErrors parsed
    in
    { title = "Turtle"
    , body =
        [ Html.div []
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
                [ Html.Attributes.style "width" "600px"
                , Html.Attributes.style "height" "600px"
                , Html.Attributes.style "float" "left"
                , Html.Events.onInput CommandsChange
                , Html.Attributes.value <| String.join "\n" model.commands
                ]
                []
            , Collage.Render.svg <|
                Collage.group <|
                    ((drawShape <| Collage.rectangle 600 600)
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
        ]
    }


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


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
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


lineStyle : Collage.LineStyle
lineStyle =
    let
        default =
            Collage.defaultLineStyle
    in
    { default | thickness = Collage.verythin }


drawShape : Collage.Shape -> Collage.Collage Msg
drawShape shape =
    shape |> Collage.outlined lineStyle


drawPath : Collage.Path -> Collage.Collage Msg
drawPath path =
    path |> Collage.traced lineStyle


stringToFloat : String -> Result Error Float
stringToFloat str =
    -- This function is needed to "map" the error to our own type, that we'll
    -- use with i18n. We also want to keep the faulty string for our final
    -- translated message.
    String.toFloat str
        |> Result.fromMaybe str
        |> Result.mapError ParseFloatError


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
splitMovesFromErrors moveAndErrorList =
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
                                (ErrorMessage (i + 1) error :: errors)
                                moves
                                tail
    in
    splitMovesFromErrors_ [] [] moveAndErrorList


toPath :
    Point
    -> Angle
    -> Bool
    -> Step
    -> { point : Point, angle : Angle, draw : Bool, path : Maybe Collage.Path }
toPath (( x, y ) as currentPoint) currentAngle draw step =
    case step of
        Forward amount ->
            let
                newPoint =
                    ( x + cos (degrees currentAngle) * amount
                    , y + sin (degrees currentAngle) * amount
                    )
            in
            { point = newPoint
            , angle = currentAngle
            , draw = draw
            , path = Just <| Collage.segment currentPoint newPoint
            }

        Left angle ->
            { point = currentPoint
            , angle = currentAngle + angle
            , draw = draw
            , path = Nothing
            }

        Right angle ->
            { point = currentPoint
            , angle = currentAngle - angle
            , draw = draw
            , path = Nothing
            }

        PenUp ->
            { point = currentPoint
            , angle = currentAngle
            , draw = False
            , path = Nothing
            }

        PenDown ->
            { point = currentPoint
            , angle = currentAngle
            , draw = True
            , path = Nothing
            }


movesToPaths : List Step -> List Collage.Path
movesToPaths steps =
    let
        movesToPaths_ :
            Point
            -> Angle
            -> Bool
            -> List Collage.Path
            -> List Step
            -> List Collage.Path
        movesToPaths_ startPoint startAngle startDraw paths moves =
            case moves of
                head :: tail ->
                    let
                        { point, angle, draw, path } =
                            toPath startPoint startAngle startDraw head
                    in
                    case path of
                        Nothing ->
                            movesToPaths_ point angle draw paths tail

                        Just segment ->
                            movesToPaths_
                                point
                                angle
                                draw
                                (if draw then
                                    segment :: paths

                                 else
                                    paths
                                )
                                tail

                [] ->
                    paths
    in
    movesToPaths_ ( 0, 0 ) 90 True [] steps
