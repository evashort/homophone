module CBool where

type alias CBool = Int

cFalse : CBool
cFalse = 0

cTrue : CBool
cTrue = 1

cBool : Bool -> CBool
cBool x = if x then cTrue else cFalse
