# Homophone Generator
Finds a different set of words that sound like the input

Single-page client-side web app written in Elm

Try it out at http://homophone.me/

## Building from source

1. Install [Elm Platform 0.18](https://guide.elm-lang.org/install.html)

1. Run `git clone https://github.com/evanshort73/elm-pairing-heap.git` so that
`elm-pairing-heap` and `homophone` are side-by-side in the same directory

1. Run `elm-make src/Main.elm --output elm.js --warn`

1. Open `index.html` in Firefox (Chrome prevents it from loading the data
files)

## Running tests

1. Install [Node.js v6.x.x LTS](https://nodejs.org/en/)

1. Run `npm install -g elm-test`

1. From the `homophone` directory, run `elm-test`

## Generating data files

1. Install [Python 3](https://www.python.org/downloads/)

1. Run `python getData.py` to populate the `cache/` directory. The total size
will be 1.17 GB. If it gets interrupted, delete whichever file it was in the
middle of generating and run it again.

1. After editing files in the `handcraft/` directory, delete certain files in
the `data/` and `cache/` directories and run `python getData.py` again. Refer
to the individual `handcraft/` files for more information.
