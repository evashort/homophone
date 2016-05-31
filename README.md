# homophone generator
finds a different set of words that sound like the input

to contribute you will need to install elm 0.16 and python 2.7

to build or run tests on windows, first run `chcp 65001` so that your shell can handle the zero-width space character in `src/Main.elm`

to build `index.html`: `elm-make src/Main.elm --output index.html`

to run tests, first `npm install -g elm-test`, then run `elm-test tests/TestRunner.elm`

to regenerate data: delete old files in `data/` and `cache/`, then run `python getData.py`
