module Tests exposing (all)

import Test exposing (..)

import DAGTest
import HomophoneTest

all : Test
all =
  concat
    [ DAGTest.dagTest
    , HomophoneTest.homophoneTest
    ]
