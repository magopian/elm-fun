port module Main exposing (..)

import Html
import Html.Attributes
import Html.Events


-- Model


type Msg
    = SaveCredentials
    | UsernameChange String
    | PasswordChange String
    | EncodedAuth String


type alias Model =
    { username : String
    , password : String
    , encoded : String
    , auth : Maybe String
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    Model "" "" "" Nothing Nothing ! []



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveCredentials ->
            if checkCredentials model.username model.password then
                { model
                    | auth = Just <| authFromCredentials model.encoded
                    , error = Nothing
                }
                    ! []
            else
                { model
                    | auth = Nothing
                    , error = Just "Wrong credentials"
                }
                    ! []

        UsernameChange username ->
            { model | username = username }
                ! [ b64encode <| username ++ ":" ++ model.password ]

        PasswordChange password ->
            { model | password = password }
                ! [ b64encode <| model.username ++ ":" ++ password ]

        EncodedAuth encoded ->
            { model | encoded = encoded } ! []


checkCredentials : String -> String -> Bool
checkCredentials username password =
    True


authFromCredentials : String -> String
authFromCredentials encoded =
    "Basic " ++ encoded



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    encoded EncodedAuth


port b64encode : String -> Cmd msg


port encoded : (String -> msg) -> Sub msg



-- View


view : Model -> Html.Html Msg
view model =
    Html.form
        [ Html.Events.onSubmit SaveCredentials ]
        [ Html.div
            []
            [ Html.input
                [ Html.Attributes.id "id_username"
                , Html.Events.onInput UsernameChange
                ]
                []
            , Html.text " Username"
            ]
        , Html.div
            []
            [ Html.input
                [ Html.Attributes.id "id_password"
                , Html.Attributes.type_ "password"
                , Html.Events.onInput PasswordChange
                ]
                []
            , Html.text " Password"
            ]
        , Html.button [] [ Html.text "Login" ]
        , Html.div
            []
            [ Html.text <| Maybe.withDefault "" model.error ]
        ]



-- Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
