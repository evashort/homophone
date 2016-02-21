# homophone generator
finds a different set of words that sound like the input

to build `index.html`: `elm-make src/Main.elm --output index.html`

to run tests, first `npm install -g elm-test`, then run `elm-test tests/TestRunner.elm`

to regenerate data: delete old files in `data/` and `python/intermediate/`, then `cd python/` and `python getData.py`
