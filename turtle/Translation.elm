module Translation exposing (Language(..), TranslationId(..), translate)


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
                        ("Forward " ++ toString distance)
                        ("Avance " ++ toString distance)

                Left angle ->
                    TranslationSet
                        ("Left " ++ toString angle)
                        ("Gauche " ++ toString angle)

                Right angle ->
                    TranslationSet
                        ("Right " ++ toString angle)
                        ("Droite " ++ toString angle)

                PenUp ->
                    TranslationSet "PenUp" "LeveStylo"

                PenDown ->
                    TranslationSet "PenDown" "BaisseStylo"

                ParseFloatError line str ->
                    TranslationSet
                        ("Line "
                            ++ (toString line)
                            ++ ": the string '"
                            ++ str
                            ++ "' doesn't look like a valid number"
                        )
                        ("Ligne "
                            ++ (toString line)
                            ++ " : la chaîne '"
                            ++ str
                            ++ "' n'a pas l'air d'un nombre"
                        )

                ParseCommandError line str ->
                    TranslationSet
                        ("Line "
                            ++ (toString line)
                            ++ ": could not understand the command '"
                            ++ str
                            ++ "'"
                        )
                        ("Ligne "
                            ++ (toString line)
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
