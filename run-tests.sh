#!/bin/sh

set -ex

elm-make --warn --docs /dev/null
elm-make src/OutlineToolkit/Example.elm --output /dev/null
elm-test
elm-format --validate src/ tests/
