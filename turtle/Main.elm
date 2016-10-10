module Main exposing (..)

import Collage
import Element
import Html exposing (Html)
import List


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


moves : List Step
moves =
    [ Forward 100, Left 90, Forward 100, Left 90, Forward 100 ]


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


main =
    Element.toHtml <|
        Collage.collage 600 600 <|
            ((drawShape <| Collage.rect 600 600)
                :: List.map drawPath (movesToPaths moves)
            )
