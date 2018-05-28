module OutlineToolkitTests exposing (all)

import OutlineToolkit
import Test exposing (..)
import Test.Html.Selector exposing (text)
import TestContext exposing (TestContext)



-- parses entrys like "<name>: <int>", and sums the ints
-- exampleOutline


start : TestContext OutlineToolkit.Msg OutlineToolkit.Model (Cmd never)
start =
    TestContext.create
        { init = ( OutlineToolkit.init, Cmd.none )
        , update = \msg model -> ( OutlineToolkit.update msg model, Cmd.none )
        , view = OutlineToolkit.view
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
        ]
