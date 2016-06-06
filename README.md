# Homophone Generator
Finds a different set of words that sound like the input

Single-page client-side web app written in Elm

Try it out at http://homophone.me/

## Building from source

1. Install [Elm Platform 0.17](http://elm-lang.org/install)

1. Run `git clone https://github.com/evanshort73/elm-pairing-heap.git` so that
`elm-pairing-heap` and `homophone` are side-by-side in the same directory

1. Run `elm-make src/Main.elm --output elm.js`

1. Open `index.html` in Firefox (Chrome prevents it from loading the data
files)

Note: `src/Main.elm` uses a
[zero-width space character](https://en.wikipedia.org/wiki/Zero-width_space)
to separate the loading dots so that they wrap correctly. To build or run
tests on Windows, first run `chcp 65001` to switch your shell to the UTF-8
[code page](https://en.wikipedia.org/wiki/Code_page).

## Running tests

1. Install [Node.js v4.x.x LTS](https://nodejs.org/en/)

1. Run `elm-make tests/Tests.elm --output tests.js`

1. Run `node tests.js`

## Generating data files

1. Install [Python 2.7](https://www.python.org/downloads/)

1. Run `python getData.py` to populate the `cache/` directory. The total size
will be 1.17 GB. If it gets interrupted, delete whichever file it was in the
middle of generating and run it again.

1. After editing files in the `handcraft/` directory, delete certain files in
the `data/` and `cache/` directories and run `python getData.py` again. Refer
to the individual `handcraft/` files for more information.
