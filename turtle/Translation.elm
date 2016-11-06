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
    in
        case lang of
            English ->
                translationSet.english

            French ->
                translationSet.french
