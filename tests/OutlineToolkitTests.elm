module OutlineToolkitTests exposing (all)

import Expect
import Html.Attributes exposing (value)
import Json.Encode
import OutlineToolkit
import Regex
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, id, tag, text)
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
                    |> TestContext.fillIn "item-0" "New Entry" "A: 1"
                    |> keydown "item-0" enter
                    |> TestContext.fillIn "item-1" "New Entry" "B: 2"
                    |> TestContext.expectViewHas [ text "Total: 3" ]
        , test "can edit an existing entry" <|
            \() ->
                start
                    |> TestContext.fillIn "item-0" "New Entry" "A: 1"
                    |> TestContext.fillIn "item-0" "New Entry" "Z: 2"
                    |> Expect.all
                        [ TestContext.expectViewHas [ tag "input", attribute (value "Z: 2") ]
                        , TestContext.expectViewHas [ text "Total: 2" ]
                        ]
        , test "can add nested entries" <|
            \() ->
                start
                    |> TestContext.fillIn "item-0" "New Entry" "A: 1"
                    |> keydown "item-0" enter
                    |> keydown "item-1" tab
                    |> TestContext.fillIn "item-0-0" "New Entry" "A.A: 2"
                    |> Expect.all
                        [ TestContext.expectViewHas [ text "Total: 3" ]
                        , TestContext.expectView (Query.hasNot [ id "item-1" ])
                        ]
        , test "can add deeply nested entries" <|
            \() ->
                start
                    |> TestContext.fillIn "item-0" "New Entry" "A: 1"
                    |> keydown "item-0" enter
                    |> keydown "item-1" tab
                    |> TestContext.fillIn "item-0-0" "New Entry" "A.A: 2"
                    |> keydown "item-0-0" enter
                    |> keydown "item-0-1" tab
                    |> TestContext.fillIn "item-0-0-0" "New Entry" "A.A.A: 4"
                    |> keydown "item-0-0-0" enter
                    |> TestContext.fillIn "item-0-0-1" "New Entry" "A.A.B: 8"
                    |> Expect.all
                        [ TestContext.expectViewHas [ tag "input", id "item-0-0-1", attribute (value "A.A.B: 8") ]
                        , TestContext.expectViewHas [ text "Total: 15" ]
                        ]
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
