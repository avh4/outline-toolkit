module OutlineToolkit exposing (Config, Model, Msg, init, program, update, view)

{-|

@docs Config, program


## Advanced

@docs Model, Msg, init, update, view

-}

import Array exposing (Array)
import Html exposing (Html, text)
import Html.Attributes exposing (for, id)
import Html.Events exposing (onInput, onWithOptions)
import Html.Keyed
import OutlineToolkit.Tree as Tree exposing (Path, Tree)


{-| -}
type alias Config summaryData =
    { parse : String -> Result String summaryData
    , summarize : Maybe summaryData -> List summaryData -> summaryData
    }


{-| -}
program : Config summaryData -> Program Never Model Msg
program config =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view config
        }


{-| -}
type Model
    = Model
        { entries : Array (Tree String)
        }


{-| -}
init : Model
init =
    Model
        { entries = Tree.empty
        }


summarize : Config summaryData -> Array (Tree String) -> summaryData
summarize config entries =
    let
        s : String -> List summaryData -> summaryData
        s aString children =
            config.summarize
                (config.parse aString |> Result.toMaybe)
                children
    in
    Array.toList entries
        |> List.map (Tree.fold s)
        |> config.summarize Nothing


{-| -}
type Msg
    = OnInputEntry (List Int) String


{-| -}
update : Msg -> Model -> Model
update msg (Model model) =
    Model <|
        case msg of
            OnInputEntry path newValue ->
                { model
                    | entries = Tree.setAt path newValue model.entries
                }


viewEntries : List (Tree String) -> Html Msg
viewEntries entries =
    let
        f : Int -> List Int -> String -> List ( String, Html Msg ) -> ( String, Html Msg )
        f i path value children =
            ( "container-" ++ String.join "-" (List.map toString (i :: path))
            , Html.Keyed.node "div"
                []
                (viewEntryInput (i :: path) value
                    :: children
                )
            )
    in
    List.indexedMap (\i -> Tree.indexedFold (f i)) entries
        |> Html.Keyed.node "div" []


{-| -}
view : Config summaryData -> Model -> Html Msg
view config (Model model) =
    Html.div []
        [ viewEntries (Array.toList model.entries)
        , viewEntryInput [ Array.length model.entries ] "" |> Tuple.second
        , viewSummary (summarize config model.entries)
        ]


viewEntryInput : List Int -> String -> ( String, Html Msg )
viewEntryInput path value =
    let
        inputId =
            "item-" ++ String.join "-" (List.map toString path)
    in
    ( inputId
    , Html.div []
        [ Html.label
            [ for inputId ]
            [ text "New Entry" ]
        , Html.input
            [ id inputId
            , Html.Attributes.value value
            , onInput (OnInputEntry path)
            ]
            []
        ]
    )


viewSummary : summaryData -> Html msg
viewSummary data =
    Html.text ("Total: " ++ toString data)
