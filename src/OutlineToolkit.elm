module OutlineToolkit exposing (Config, Model, Msg, init, program, update, view)

{-|

@docs Config, program


## Advanced

@docs Model, Msg, init, update, view

-}

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes exposing (class, for, id, style)
import Html.Events exposing (onInput, onWithOptions)
import Html.Keyed
import Json.Decode
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
    | OnTab (List Int)


{-| -}
update : Msg -> Model -> Model
update msg (Model model) =
    Model <|
        case msg of
            OnInputEntry path newValue ->
                { model
                    | entries = Tree.set path newValue model.entries
                }

            OnTab path ->
                { model
                    | entries =
                        model.entries
                            |> Tree.findOrCreate path ""
                            |> Tree.indent path
                }


viewEntries : Array (Tree String) -> Html Msg
viewEntries entries =
    let
        f : Int -> List Int -> String -> List ( String, Html Msg ) -> ( String, Html Msg )
        f i path value children =
            let
                containerId =
                    "container-" ++ String.join "-" (List.map toString (i :: path))
            in
            ( containerId
            , Html.Keyed.node "div"
                [ class "container"
                , id containerId
                , style
                    [ ( "border", "1px solid black" )
                    , ( "padding", "10px" )
                    ]
                ]
                (viewEntryInput (i :: path) value
                    :: children
                )
            )
    in
    entries
        |> Array.toList
        |> List.indexedMap (\i -> Tree.indexedFold (f i))
        |> Html.Keyed.node "div" []


{-| -}
view : Config summaryData -> Model -> Html Msg
view config (Model model) =
    Html.div []
        [ model.entries
            |> Tree.findOrCreate [ Array.length model.entries ] ""
            |> viewEntries
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
            , Html.Attributes.defaultValue value
            , Html.Attributes.value value
            , onInput (OnInputEntry path)
            , onWithOptions "keydown"
                { preventDefault = True
                , stopPropagation = True
                }
                (decodeKey (Dict.fromList [ ( 9, OnTab path ) ]))
            ]
            []
        ]
    )


decodeKey : Dict Int msg -> Json.Decode.Decoder msg
decodeKey mappings =
    let
        mapKey key =
            case Dict.get key mappings of
                Nothing ->
                    Json.Decode.fail ""

                Just msg ->
                    Json.Decode.succeed msg
    in
    Json.Decode.at [ "keyCode" ] Json.Decode.int
        |> Json.Decode.andThen mapKey


viewSummary : summaryData -> Html msg
viewSummary data =
    Html.text ("Total: " ++ toString data)
