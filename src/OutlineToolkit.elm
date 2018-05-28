module OutlineToolkit exposing (Config, Model, Msg, init, program, update, view)

{-|

@docs Config, program


## Advanced

@docs Model, Msg, init, update, view

-}

import Array exposing (Array)
import Html exposing (Html, text)
import Html.Attributes exposing (for, id)
import Html.Events exposing (onInput)
import Html.Keyed


{-| -}
type alias Config summaryData =
    { parse : String -> Result String summaryData
    , summarize : List summaryData -> summaryData
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
        { entries : Array String
        }


{-| -}
init : Model
init =
    Model
        { entries = Array.empty
        }


summarize : Config summaryData -> Array String -> summaryData
summarize config entries =
    Array.toList entries
        |> List.filterMap (config.parse >> Result.toMaybe)
        |> config.summarize


type alias Path =
    Int


{-| -}
type Msg
    = OnInputEntry Path String


{-| -}
update : Msg -> Model -> Model
update msg (Model model) =
    Model <|
        case msg of
            OnInputEntry path newValue ->
                { model
                    | entries =
                        model.entries
                            |> createIfNecessary path
                            |> Array.set path newValue
                }


createIfNecessary : Path -> Array String -> Array String
createIfNecessary path array =
    let
        neededItems =
            (path + 1) - Array.length array
    in
    if neededItems <= 0 then
        array

    else
        Array.append array (Array.repeat neededItems "")


{-| -}
view : Config summaryData -> Model -> Html Msg
view config (Model model) =
    Html.div []
        [ Html.Keyed.node "div"
            []
            (List.concat
                [ model.entries
                    |> Array.toList
                    |> List.indexedMap (\i value -> viewEntryInput (i + 1))
                , [ viewEntryInput (Array.length model.entries + 1) ]
                ]
            )
        , viewSummary (summarize config model.entries)
        ]


viewEntryInput : Int -> ( String, Html Msg )
viewEntryInput i =
    let
        inputId =
            "item-" ++ toString i
    in
    ( inputId
    , Html.div []
        [ Html.label
            [ for inputId ]
            [ text "New Entry" ]
        , Html.input
            [ id inputId
            , onInput (OnInputEntry (i - 1))
            ]
            []
        ]
    )


viewSummary : summaryData -> Html msg
viewSummary data =
    Html.text ("Total: " ++ toString data)
