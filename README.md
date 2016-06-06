# Homophone Generator
Finds a different set of words that sound like the input

Single-page client-side web app written in Elm

Try it out at http://homophone.me/

## Building from source

1. Install [Elm Platform 0.17](http://elm-lang.org/install)

1. Run `git clone https://github.com/evanshort73/elm-pairing-heap.git` so that
`elm-pairing-heap` and `homophone` are side-by-side in the same directory

1. Install [Node.js v4.x.x LTS](https://nodejs.org/en/)

1. Install [Python 2.7](https://www.python.org/downloads/)

To build `index.html`: `elm-make src/Main.elm`

Note: `src/Main.elm` uses a
[zero-width space character](https://en.wikipedia.org/wiki/Zero-width_space)
to separate the loading dots so that they wrap correctly. To build or run
tests on Windows, first run `chcp 65001` to switch your shell to the UTF-8
[code page](https://en.wikipedia.org/wiki/Code_page).

To run tests: `elm-make tests/Tests.elm --output tests.js` and `node tests.js`

To regenerate data, first delete the outdated files in `data/` and `cache/`,
then run `python getData.py`. For more information, refer to the comments in
each file in the `handcraft/` directory.
