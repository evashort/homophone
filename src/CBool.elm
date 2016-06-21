module CBool exposing (CBool, cFalse, cTrue, cBool, toBool)

-- Bools aren't comparable, but CBools are

type alias CBool = Int

cFalse : CBool
cFalse = 0

cTrue : CBool
cTrue = 1

cBool : Bool -> CBool
cBool x = if x then cTrue else cFalse

toBool : CBool -> Bool
toBool cBool = cBool /= cFalse
