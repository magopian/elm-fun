module Main exposing (..)

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
    | DrawHouse
    | DrawStar
    | DrawElm


type alias Model =
    { commands : List String
    }


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
    , "Forward 150"
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
    , "Forward 120"
    , "Left 90"
    , "Forward 100"
    , "Left 180"
    , "Forward 100"
    , "Left 90"
    , "Forward 100"
    , "Left 90"
    , "Forward 100"
    , "Right 140"
    , "Forward 60"
    , "Left 100"
    , "Forward 60"
    , "Right 140"
    , "Forward 100"
    ]


initialModel : Model
initialModel =
    { commands = house
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CommandsChange commands ->
            { model | commands = String.split "\n" commands }

        DrawHouse ->
            { model | commands = house }

        DrawStar ->
            { model | commands = star }

        DrawElm ->
            { model | commands = elm }



-- View


view : Model -> Html Msg
view model =
    let
        parsed =
            commandsToMoves model.commands

        errors =
            parsed
                |> List.filter
                    (\( i, res ) ->
                        case res of
                            Ok _ ->
                                False

                            Err _ ->
                                True
                    )
                |> List.map
                    (\( i, res ) ->
                        case res of
                            Ok _ ->
                                Debug.crash "we filtered, we can't be here"

                            Err msg ->
                                "Line " ++ (toString i) ++ ": " ++ msg
                    )
                |> String.join "\n"

        moves =
            parsed
                |> List.filter
                    (\( _, res ) ->
                        case res of
                            Ok move ->
                                True

                            Err _ ->
                                False
                    )
                |> List.map
                    (\( _, res ) ->
                        case res of
                            Ok move ->
                                move

                            Err _ ->
                                Debug.crash "we filtered, we can't be here"
                    )
    in
        Html.div []
            [ Html.textarea
                [ Html.Attributes.style
                    [ ( "width", "600px" )
                    , ( "height", "600px" )
                    , ( "float", "left" )
                    ]
                , Html.Events.onInput CommandsChange
                ]
                [ Html.text <| String.join "\n" model.commands ]
            , Element.toHtml <|
                Collage.collage 600 600 <|
                    ((drawShape <| Collage.rect 600 600)
                        :: List.map drawPath (movesToPaths moves)
                    )
            , Html.button
                [ Html.Events.onClick DrawHouse ]
                [ Html.text "house" ]
            , Html.button
                [ Html.Events.onClick DrawStar ]
                [ Html.text "star" ]
            , Html.button
                [ Html.Events.onClick DrawElm ]
                [ Html.text "Elm" ]
            , Html.pre [] [ Html.text errors ]
            ]



-- Main


main =
    Html.App.beginnerProgram
        { view = view
        , model = initialModel
        , update = update
        }



-- Helpers


startingPoint =
    ( 0, 0 )


startingAngle =
    90


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

        _ ->
            Err "Could not parse the command"


commandsToMoves : List String -> List ( Int, Result String Step )
commandsToMoves commands =
    List.indexedMap
        (\i command ->
            ( i, parseCommand command )
        )
        commands


toPath : Point -> Angle -> Step -> ( Point, Angle, Maybe Collage.Path )
toPath (( x, y ) as currentPoint) currentAngle step =
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
                , Just <| Collage.segment currentPoint newPoint
                )

        Left angle ->
            ( currentPoint, currentAngle + angle, Nothing )

        Right angle ->
            ( currentPoint, currentAngle - angle, Nothing )


movesToPaths : List Step -> List Collage.Path
movesToPaths moves =
    let
        movesToPaths' :
            Point
            -> Angle
            -> List Collage.Path
            -> List Step
            -> List Collage.Path
        movesToPaths' point angle paths moves =
            case moves of
                head :: tail ->
                    let
                        ( newPoint, newAngle, path ) =
                            toPath point angle head
                    in
                        case path of
                            Nothing ->
                                movesToPaths' newPoint newAngle paths tail

                            Just segment ->
                                movesToPaths' newPoint newAngle (segment :: paths) tail

                [] ->
                    paths
    in
        movesToPaths' startingPoint startingAngle [] moves
