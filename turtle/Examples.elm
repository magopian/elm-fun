module Examples exposing (house, star, elm)

import Translation as T

house : T.Language -> List String
house lang =
    [ T.Forward 100
    , T.Right 135
    , T.Forward 141.42
    , T.Left 135
    , T.Forward 100
    , T.Left 90
    , T.Forward 100
    , T.Right 135
    , T.Forward 70.71
    , T.Right 90
    , T.Forward 70.71
    , T.Right 90
    , T.Forward 141.42
    , T.Left 135
    , T.Forward 100
    ]
        |> List.map (T.translate lang)


star : T.Language -> List String
star lang =
    [ T.Forward 100
    , T.Right 144
    , T.Forward 100
    , T.Right 144
    , T.Forward 100
    , T.Right 144
    , T.Forward 100
    , T.Right 144
    , T.Forward 100
    ]
        |> List.map (T.translate lang)


elm : T.Language -> List String
elm lang =
    [ T.Left 90
    , T.PenUp
    , T.Forward 100
    , T.PenDown
    , T.Forward 50
    , T.Right 90
    , T.Forward 100
    , T.Right 90
    , T.Forward 50
    , T.Left 180
    , T.Forward 50
    , T.Left 90
    , T.Forward 50
    , T.Left 90
    , T.Forward 40
    , T.Left 180
    , T.Forward 40
    , T.Left 90
    , T.Forward 50
    , T.Left 90
    , T.Forward 50
    , T.PenUp
    , T.Forward 50
    , T.PenDown
    , T.Left 90
    , T.Forward 100
    , T.Left 180
    , T.Forward 100
    , T.Left 90
    , T.Forward 50
    , T.PenUp
    , T.Forward 50
    , T.PenDown
    , T.Left 90
    , T.Forward 100
    , T.Right 140
    , T.Forward 60
    , T.Left 100
    , T.Forward 60
    , T.Right 140
    , T.Forward 100
    ]
        |> List.map (T.translate lang)


