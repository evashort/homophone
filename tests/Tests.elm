import ElmTest exposing (..)

import DAGTest
import HomophoneTest

main : Program Never
main = runSuite all

all : Test
all =
  suite "Everything Suite"
    [ DAGTest.dagTest
    , HomophoneTest.homophoneTest
    ]
