module Translation exposing
    ( Language(..)
    , TranslationId(..)
    , codeToLanguage
    , translate
    )

import Dict


type Language
    = English
    | French


type alias TranslationSet =
    { english : String
    , french : String
    }


type TranslationId
    = TurtleCommands
    | DisplayTurtle
    | ShareUrl
    | House
    | Star
    | Elm
    | Forward Float
    | Left Float
    | Right Float
    | PenUp
    | PenDown
    | ParseCommandError Int String
    | ParseFloatError Int String


codeToLanguage : String -> Language
codeToLanguage languageCode =
    Dict.fromList [ ( "en", English ), ( "fr", French ) ]
        |> Dict.get languageCode
        |> Maybe.withDefault English


translate : Language -> TranslationId -> String
translate lang trans =
    let
        translationSet =
            case trans of
                TurtleCommands ->
                    TranslationSet
                        """Turtle commands:
                        Forward <distance>,
                        Left <angle>,
                        Right <angle>,
                        PenUp,
                        PenDown
                        """
                        """Commandes de la tortue :
                        Avance <distance>,
                        Gauche <angle>,
                        Droite <angle>,
                        LeveStylo,
                        BaisseStylo
                        """

                DisplayTurtle ->
                    TranslationSet
                        "Display the 'turtle'?"
                        "Afficher la 'tortue' ?"

                ShareUrl ->
                    TranslationSet "Share url" "Lien de partage"

                House ->
                    TranslationSet "House" "Maison"

                Star ->
                    TranslationSet "Star" "Étoile"

                Elm ->
                    TranslationSet "Elm" "Elm"

                Forward distance ->
                    TranslationSet
                        ("Forward " ++ String.fromFloat distance)
                        ("Avance " ++ String.fromFloat distance)

                Left angle ->
                    TranslationSet
                        ("Left " ++ String.fromFloat angle)
                        ("Gauche " ++ String.fromFloat angle)

                Right angle ->
                    TranslationSet
                        ("Right " ++ String.fromFloat angle)
                        ("Droite " ++ String.fromFloat angle)

                PenUp ->
                    TranslationSet "PenUp" "LeveStylo"

                PenDown ->
                    TranslationSet "PenDown" "BaisseStylo"

                ParseFloatError line str ->
                    TranslationSet
                        ("Line "
                            ++ String.fromInt line
                            ++ ": the string '"
                            ++ str
                            ++ "' doesn't look like a valid number"
                        )
                        ("Ligne "
                            ++ String.fromInt line
                            ++ " : la chaîne '"
                            ++ str
                            ++ "' n'a pas l'air d'un nombre"
                        )

                ParseCommandError line str ->
                    TranslationSet
                        ("Line "
                            ++ String.fromInt line
                            ++ ": could not understand the command '"
                            ++ str
                            ++ "'"
                        )
                        ("Ligne "
                            ++ String.fromInt line
                            ++ " : impossible de comprendre la commande '"
                            ++ str
                            ++ "'"
                        )
    in
    case lang of
        English ->
            translationSet.english

        French ->
            translationSet.french
