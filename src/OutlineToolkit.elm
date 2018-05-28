module OutlineToolkit exposing (Model, Msg, init, update, view)

{-|


## Advanced

@docs Model, Msg, init, update, view

-}

import Html exposing (Html, text)
import Html.Attributes exposing (for, id)
import Html.Events exposing (onInput)


{-| -}
type Model
    = Model ()


{-| -}
init : Model
init =
    Model ()


{-| -}
type Msg
    = OnInputEntry String


{-| -}
update : Msg -> Model -> Model
update msg model =
    model


{-| -}
view : Model -> Html Msg
view model =
    Html.div []
        [ viewEntryInput 1
        , viewEntryInput 2
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
            , onInput OnInputEntry
            ]
            []
        ]
