module OutlineToolkit.Example exposing (main)

import OutlineToolkit
import Regex


main : Program Never OutlineToolkit.Model OutlineToolkit.Msg
main =
    OutlineToolkit.program
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
