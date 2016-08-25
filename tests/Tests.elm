import ElmTest exposing (..)

import HomophoneTest

main : Program Never
main = runSuite all

all : Test
all =
  suite "Everything Suite"
    [ HomophoneTest.homophoneTest
    ]
