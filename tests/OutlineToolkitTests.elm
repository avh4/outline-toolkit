module OutlineToolkitTests exposing (all)

import Json.Encode
import OutlineToolkit
import Regex
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (id, tag, text)
import TestContext exposing (TestContext)


{-| This test outline program allows entries of the form
"<name>: <int>", and sums the ints.
-}
exampleOutline : OutlineToolkit.Config Int
exampleOutline =
    { parse =
        \string ->
            case
                string
                    |> Regex.find Regex.All (Regex.regex "(.*): ([0-9]+)")
                    |> List.map .submatches
            of
                [ [ Just _, Just int ] ] ->
                    case String.toInt int of
                        Ok i ->
                            Ok i

                        Err message ->
                            Err message

                _ ->
                    Err "Failed to parse"
    , summarize =
        \value children ->
            Maybe.withDefault 0 value
                + List.sum children
    }


start : TestContext OutlineToolkit.Msg OutlineToolkit.Model (Cmd OutlineToolkit.Msg)
start =
    TestContext.create
        { init = ( OutlineToolkit.init, Cmd.none )
        , update = OutlineToolkit.update
        , view = OutlineToolkit.view exampleOutline
        }


all : Test
all =
    describe "outline-toolkit"
        [ test "can compute aggregate information about the entries" <|
            \() ->
                start
                    |> TestContext.fillIn "item-0" "New Entry" "A: 7"
                    |> keydown "item-0" enter
                    |> TestContext.fillIn "item-1" "New Entry" "B: 3"
                    |> TestContext.expectViewHas [ text "Total: 10" ]
        , test "can edit an existing entry" <|
            \() ->
                start
                    |> TestContext.fillIn "item-0" "New Entry" "A: 7"
                    |> TestContext.fillIn "item-0" "New Entry" "Z: 70"
                    -- ideally we would check the value in the input,
                    -- but the value is only in the real DOM, not in the virtual DOM
                    -- |> TestContext.expectViewHas [ tag "input", text "Z: 70" ]
                    |> TestContext.expectViewHas [ text "Total: 70" ]
        , test "can add nested entries" <|
            \() ->
                start
                    |> TestContext.fillIn "item-0" "New Entry" "A: 7"
                    |> keydown "item-0" enter
                    |> keydown "item-1" tab
                    |> TestContext.fillIn "item-0-0" "New Entry" "A.A: 10"
                    |> TestContext.expectViewHas [ text "Total: 17" ]
        ]


tab : Int
tab =
    9


enter : Int
enter =
    13


keydown : String -> Int -> TestContext msg model effect -> TestContext msg model effect
keydown fieldId key =
    TestContext.simulate
        (Query.find [ id fieldId ])
        ( "keydown"
        , Json.Encode.object [ ( "keyCode", Json.Encode.int key ) ]
        )
