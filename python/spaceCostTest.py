from repronounce import getSpaceCosts

c = getSpaceCosts(["qq", "www", "qqq"], "qqwwqqq")
assert c == (0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0)
print "it can create a gray area smaller than the replaced word"

c = getSpaceCosts(["qq", "ww", "qqq"], "qqwwwqqq")
assert c == (0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0)
print "it can create a gray area larger than the replaced word"

c = getSpaceCosts(["qq", "ww", "w", "qqq"], "qqwwwqqq")
assert c == (0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0)
print "it can replace multiple words"

c = getSpaceCosts(["", "qq", "ww", "qqq"], "qqwwqqq")
assert c == (1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0)
print "if there is a space on the initial boundary, the gray area starts there"

c = getSpaceCosts(["qq", "ww", "qqq", ""], "qqwwqqq")
assert c == (0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0)
print "if there is a space on the final boundary, the gray area ends there"

c = getSpaceCosts(["", "qq", "ww", "qqq", ""], "qqwwqqq")
assert c == (1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0)
print "if there is a space on both boundaries, the gray area is everywhere"

c = getSpaceCosts(["qq", "ww", "qqq"], "qqqqq")
assert c == (0.0, 1.0, 0.0, 0.0, 0.0)
print "it creates a length-1 gray area for a deleted word"

c = getSpaceCosts(["qq", "ww", "qqq"], "qqqq")
assert c == (0.0, 0.0, 0.0, 0.0)
print "if the gray area would have negative length, it omits it"

c = getSpaceCosts(["qq", "qqq"], "qqwwqqq")
assert c == (0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0)
print "it can create a gray area without a replaced word"

c = getSpaceCosts(["qq", "qqq"], "qqqqq")
assert c == (0.0, 1.0, 0.0, 0.0, 0.0)
print "it can create a length-1 gray area without a replaced word"

c = getSpaceCosts(["qq", "qqq"], "q")
assert c == (0.0,)
print "it can ignore a negative-length gray area without a replaced word"

c = getSpaceCosts(["qqqqq"], "q")
assert c == (0.0,)
print "if it's shortened and has no spaces, there's no cost"

c = getSpaceCosts(["q"], "qqqqq")
assert c == (0.0, 0.0, 0.0, 0.0, 0.0)
print "if it's lengthened and has no spaces, there's no cost"

c = getSpaceCosts(["qqqqq", ""], "qq")
assert c == (0.0, 2.0,)
print "if it's shortened and ends with a space, there's no gray area"

c = getSpaceCosts(["qq", ""], "qqqqq")
assert c == (0.0, 1.0, 1.0, 1.0, 2.0,)
print "if it's lengthened and ends with a space, there's a gray area"

c = getSpaceCosts(["", "qqqqq"], "qq")
assert c == (0.0, 0.0,)
print "if it's shortened and starts with a space, there's no gray area"

c = getSpaceCosts(["", "qq"], "qqqqq")
assert c == (1.0, 1.0, 1.0, 0.0, 0.0)
print "if it's lengthened and starts with a space, there's a gray area"

c = getSpaceCosts(["", "qqqqq", ""], "qq")
assert c == (1.0, 2.0,)
print "if it's shortened and has spaces on both sides, it's all gray area"

c = getSpaceCosts(["", "qq", ""], "qqqqq")
assert c == (1.0, 1.0, 1.0, 1.0, 2.0)
print "if it's lengthened and has spaces on both sides, it's all gray area"

c = getSpaceCosts([""], "qqq")
assert c == (0.0, 0.0, 0.0)
print "rabbits without a space have no cost"

c = getSpaceCosts(["", ""], "qqq")
assert c == (2.0, 2.0, 2.0)
print "rabbits with a space are all cost"
