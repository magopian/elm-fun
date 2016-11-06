module Main exposing (..)

import Base64
import Collage
import Element
import Html exposing (Html)
import Html.App
import Html.Attributes
import Html.Events
import List
import String


-- Update


type Msg
    = CommandsChange String
    | LoadHouse
    | LoadStar
    | LoadElm
    | DrawTurtle Bool
    | SetLanguage Language


type alias Model =
    { commands : List String
    , drawTurtle : Bool
    , lang : Language
    }


type Language
    = English
    | French


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


house =
    [ "Forward 100"
    , "Right 135"
    , "Forward 141.42"
    , "Left 135"
    , "Forward 100"
    , "Left 90"
    , "Forward 100"
    , "Right 135"
    , "Forward 70.71"
    , "Right 90"
    , "Forward 70.71"
    , "Right 90"
    , "Forward 141.42"
    , "Left 135"
    , "Forward 100"
    ]


star =
    [ "Forward 100"
    , "Right 144"
    , "Forward 100"
    , "Right 144"
    , "Forward 100"
    , "Right 144"
    , "Forward 100"
    , "Right 144"
    , "Forward 100"
    ]


elm =
    [ "Left 90"
    , "PenUp"
    , "Forward 100"
    , "PenDown"
    , "Forward 50"
    , "Right 90"
    , "Forward 100"
    , "Right 90"
    , "Forward 50"
    , "Left 180"
    , "Forward 50"
    , "Left 90"
    , "Forward 50"
    , "Left 90"
    , "Forward 40"
    , "Left 180"
    , "Forward 40"
    , "Left 90"
    , "Forward 50"
    , "Left 90"
    , "Forward 50"
    , "PenUp"
    , "Forward 50"
    , "PenDown"
    , "Left 90"
    , "Forward 100"
    , "Left 180"
    , "Forward 100"
    , "Left 90"
    , "Forward 50"
    , "PenUp"
    , "Forward 50"
    , "PenDown"
    , "Left 90"
    , "Forward 100"
    , "Right 140"
    , "Forward 60"
    , "Left 100"
    , "Forward 60"
    , "Right 140"
    , "Forward 100"
    ]


type alias Flags =
    { hash : String }


init : Flags -> ( Model, Cmd Msg )
init { hash } =
    case hash of
        "" ->
            (Model house True English) ! []

        hash ->
            case Base64.decode hash of
                Ok commands ->
                    (Model (String.split "\n" commands) True English) ! []

                Err msg ->
                    let
                        _ =
                            Debug.log "failed to decode hash" msg
                    in
                        (Model house True English) ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CommandsChange commands ->
            { model | commands = String.split "\n" commands } ! []

        LoadHouse ->
            { model | commands = house } ! []

        LoadStar ->
            { model | commands = star } ! []

        LoadElm ->
            { model | commands = elm } ! []

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
                [ Html.text """Turtle commands:
                    Forward <distance>,
                    Left <angle>,
                    Right <angle>,
                    PenUp,
                    PenDown
                    """ ]
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
                    , Html.text "Display the 'turtle'?"
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
                [ Html.text "house" ]
            , Html.button
                [ Html.Events.onClick LoadStar ]
                [ Html.text "star" ]
            , Html.button
                [ Html.Events.onClick LoadElm ]
                [ Html.text "Elm" ]
            , Html.p
                []
                [ Html.a
                    [ Html.Attributes.href (urlFromCommands model.commands) ]
                    [ Html.text "Share url" ]
                ]
            , Html.pre [] [ Html.text <| String.join "\n" errors ]
            ]


languageSwitcher : Language -> Html Msg
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
            [ button' English "English"
            , button' French "Français"
            ]



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


parseCommand : String -> Result String Step
parseCommand command =
    case String.split " " command of
        [ "Forward", distance ] ->
            Result.map Forward (String.toFloat distance)

        [ "Left", angle ] ->
            Result.map Left (String.toFloat angle)

        [ "Right", angle ] ->
            Result.map Right (String.toFloat angle)

        [ "PenUp" ] ->
            Ok PenUp

        [ "PenDown" ] ->
            Ok PenDown

        _ ->
            Err "Could not parse the command"


commandsToMoves : List String -> List ( Int, Result String Step )
commandsToMoves commands =
    List.indexedMap
        (\i command ->
            ( i, parseCommand command )
        )
        commands


splitMovesFromErrors :
    List ( Int, Result String Step )
    -> ( List String, List Step )
splitMovesFromErrors movesAndErrors =
    let
        splitMovesFromErrors' :
            List String
            -> List Step
            -> List ( Int, Result String Step )
            -> ( List String, List Step )
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

                        ( i, Err msg ) ->
                            splitMovesFromErrors'
                                (("Line " ++ (toString i) ++ ": " ++ msg)
                                    :: errors
                                )
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


urlFromCommands : List String -> String
urlFromCommands commands =
    "#"
        ++ (commands
                |> String.join "\n"
                |> Base64.encode
                |> Result.withDefault ""
           )
