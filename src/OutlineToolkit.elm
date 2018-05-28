module OutlineToolkit exposing (Model, Msg, init, update, view)

{-|


## Advanced

@docs Model, Msg, init, update, view

-}

import Array exposing (Array)
import Html exposing (Html, text)
import Html.Attributes exposing (for, id)
import Html.Events exposing (onInput)


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
view : Model -> Html Msg
view (Model model) =
    Html.div []
        [ viewEntryInput (Array.length model.entries + 1)
        , text "Total: 10"
        ]


viewEntryInput : Int -> Html Msg
viewEntryInput i =
    let
        inputId =
            "item-" ++ toString i
    in
    Html.div []
        [ Html.label
            [ for inputId ]
            [ text "New Entry" ]
        , Html.input
            [ id inputId
            , onInput (OnInputEntry (i - 1))
            ]
            []
        ]
