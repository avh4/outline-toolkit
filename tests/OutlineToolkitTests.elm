module OutlineToolkitTests exposing (all)

import OutlineToolkit
import Regex
import Test exposing (..)
import Test.Html.Selector exposing (tag, text)
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
        \children ->
            List.sum children
    }


start : TestContext OutlineToolkit.Msg OutlineToolkit.Model (Cmd never)
start =
    TestContext.create
        { init = ( OutlineToolkit.init, Cmd.none )
        , update = \msg model -> ( OutlineToolkit.update msg model, Cmd.none )
        , view = OutlineToolkit.view exampleOutline
        }


all : Test
all =
    describe "outline-toolkit"
        [ test "can compute aggregate information about the entries" <|
            \() ->
                start
                    |> TestContext.fillIn "item-1" "New Entry" "A: 7"
                    |> TestContext.fillIn "item-2" "New Entry" "B: 3"
                    |> TestContext.expectViewHas [ text "Total: 10" ]
        , test "can edit an existing item" <|
            \() ->
                start
                    |> TestContext.fillIn "item-1" "New Entry" "A: 7"
                    |> TestContext.fillIn "item-1" "New Entry" "Z: 70"
                    -- ideally we would check the value in the input,
                    -- but the value is only in the real DOM, not in the virtual DOM
                    -- |> TestContext.expectViewHas [ tag "input", text "Z: 70" ]
                    |> TestContext.expectViewHas [ text "Total: 70" ]
        ]
