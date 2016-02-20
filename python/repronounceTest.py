from repronounce import getSentence

wbp = 2.0
swbp = 1.0

state = getSentence(
    [["ar"]],
    [("abca", (0.0,)), ("bcab", (0.0,)), ("cabcar", (0.0,))],
    [("", {"abc": 0.0})],
    [])
cost, b = state
assert cost == 3 * wbp
assert b == ["abca", "bcab", "cabcar"]
print "It can repeat the same rabbit in order to create the right leftovers"

state = getSentence(
    [["ar"]],
    [("abca", (0.0,)), ("bcab", (0.0,)), ("cabca", (0.0,))],
    [("", {"abc": 0.0})],
    [])
assert state is None
print "It stops repeating the rabbit after trying all possible leftovers"

state = getSentence(
    [["ah"], ["up"]],
    [("a", (0.0,)), ("ah", (0.0,)), ("at", (0.0,)), ("p", (0.0,)), ("up", (0.0,))],
    [("p", {"t": wbp * 0.9})],
    [("h", {"": 0.0}), ("u", {"": 0.0})])
cost, b = state
assert cost == 1.9 * wbp
assert b == ["at"]
print "It can't avoid the WBP by deleting the rest of the word"

state = getSentence(
    [["x"]],
    [("a", (0.0,)), ("b", (0.0,)), ("c", (0.0,)), ("de", (0.0,)), ("f", (0.0,)), ("g", (0.0,))],
    [("", {"efg": 0.0}), ("x", {"abcd": 0.0})],
    [])
cost, b = state
assert cost ==  3 * swbp + 3 * wbp
assert b == ["a", "b", "c", "de", "f", "g"]
print "It can create multiple words within the same substitution or rabbit"

state = getSentence(
    [["a"], ["uu"], ["u"], ["uu"], ["a"]],
    [("ah", (0.0,)), ("h", (0.0,)), ("ha", (0.0,))],
    [("", {"hhh": 0.0})],
    [("u", {"": 0.0})])
cost, b = state
assert cost == 3 * wbp
assert b == ["ah", "h", "ha"]
print "When it deletes entire words, the WBP applies anywhere on the boundary"

state = getSentence(
    [["a"], ["cat"], ["o"]],
    [("ado", (0.0,)), ("go", (0.0,))],
    [("cat", {"dog": 0.0})],
    [])
cost, b = state
assert cost == swbp + wbp
assert b == ["ado", "go"]
print "When a word is fully replaced, boundaries within it get the semi-WBP"

state = getSentence(
    [["bed"], ["time"]],
    [("bet", (0.0,)), ("dime", (0.0,))],
    [("dt", {"td": 0.0})],
    [])
cost, b = state
assert cost == swbp + wbp
assert b == ["bet", "dime"]
print "Boundaries at the edges of the original words still get the semi-WBP"

state = getSentence(
    [["ab", "a"], ["a"]],
    [("abra", (0.0,)), ("aha", (0.0,))],
    [("", {"h": 0.0, "r": 1.0})],
    [])
cost, b = state
assert cost == wbp
assert b == ["aha"]
print "It can choose a pronunciation that is completed by the other one"

state = getSentence(
    [["ab", "a"], ["a"]],
    [("abra", (0.0,)), ("aha", (0.0,))],
    [("", {"h": 1.0, "r": 0.0})],
    [])
cost, b = state
assert cost == wbp
assert b == ["abra"]
print "It can choose a pronunciation that is a completion of the other one"

state = getSentence(
    [["ab", "cd"]],
    [("ad", (0.0,)), ("bc", (0.0,))],
    [],
    [])
assert state is None
print "Once it chooses a pronunciation, it can't use parts of the other one"

state = getSentence(
    [["abh"]],
    [("c", (0.0,)), ("d", (0.0,))],
    [("ab", {"cd": 0.0})],
    [("h", {"": 0.0})])
cost, b = state
assert cost == swbp + wbp
assert b == ["c", "d"]
print "It can delete even when a substitution makes the word uncontinuable"

state = getSentence(
    [["abc"]],
    [("def", (0.0,)), ("gif", (0.0,))],
    [("a", {"g": 1000.0}), ("ab", {"de": 0.0}), ("b", {"i": -2000.0}),
     ("c", {"f": 0.0})],
    [])
cost, b = state
assert cost == wbp - 1000.0
assert b == ["gif"]
print "Smaller subproblems can overtake larger cheaper ones via a reward"

state = getSentence(
    [["at"]],
    [("hhot", (0.0,))],
    [("", {"h": -1000.0}), ("a", {"o": 1.0})],
    [])
cost, b = state
assert cost == wbp - 1999.0
assert b == ["hhot"]
print "It stops accumulating reward when there are no more matching words"

state = getSentence(
    [["cot"]],
    [("cat", (1.0,)), ("cot", (2.0,))],
    [("o", {"a": 2.0})],
    [])
cost, b = state
assert cost == 2.0 + wbp
assert b == ["cot"]
print "It can choose the more expensive word to avoid a substitution"

state = getSentence(
    [["cot"]],
    [("cat", (1.0,)), ("cot", (3.0,))],
    [("o", {"a": 1.0})],
    [])
cost, b = state
assert cost == 2.0 + wbp
assert b == ["cat"]
print "It can make an expensive substitution to avoid the more exensive word"

state = getSentence(
    [["a"]],
    [("ax", (0.0,)), ("wx", (0.0,)), ("y", (0.0,)), ("z", (0.0,))],
    [("", {"xyz": 0.0}), ("a", {"wxyz": 1.5 * wbp - 1.5 * swbp})],
    [])
cost, b = state
assert cost == 2.5 * wbp + 0.5 * swbp
assert b == ["wx", "y", "z"]
print "It distinguishes puzzles by whether the last substitution is a rabbit"

state = getSentence(
    [["abcd"]],
    [("ax", (0.0,)), ("wx", (0.0,)), ("y", (0.0,)), ("z", (0.0,))],
    [("abcd", {"wxyz": 0.0}), ("bcd", {"xyz": 1.5 * swbp})],
    [])
cost, b = state
assert cost == 1.5 * swbp + wbp
assert b == ["ax", "y", "z"]
print "It distinguishes puzzles by whether the last value is in the gray area"

state = getSentence(
    [["s"], ["abc"]],
    [("a", (0.0,)), ("s", (0.0,)), ("x", (0.0,)), ("yc", (0.0,))],
    [("", {"xy": 0.0})],
    [("ab", {"": 0.0}), ("b", {"": 0.5 * wbp})])
cost, b = state
assert cost == 2.5 * wbp
assert b == ["s", "a", "x", "yc"]
print "It distinguishes puzzles by whether there is a space on the boundary"

state = getSentence(
    [["a"], ["bc"]],
    [("a", (0.0,)), ("x", (0.0,)), ("yc", (0.0,))],
    [("", {"xy": 0.0})],
    [("ab", {"": 1000.0}), ("b", {"": 0.0})])
cost, b = state
assert cost == 3 * wbp
assert b == ["a", "x", "yc"]
print "It doesn't prematurely expand puzzles created by deletions alone"

state = getSentence(
    [["le"], ["hot"], ["ax"]],
    [("dox", (0.0,)), ("la", (0.0,))],
    [("ota", {"ado": 0.0})],
    [("e", {"": 0.0}), ("h", {"": 0.0})])
cost, b = state
assert cost == swbp + wbp
assert b == ["la", "dox"]
print "If there's a space anywhere on the boundary, the gray area starts there"

state = getSentence(
    [["ox"], ["ate"], ["hi"]],
    [("one", (0.0,)), ("pi", (0.0,))],
    [("xat", {"nep": 0.0})],
    [("e", {"": 0.0}), ("h", {"": 0.0})])
cost, b = state
assert cost == swbp + wbp
assert b == ["one", "pi"]
print "If there's a space anywhere on the boundary, the gray area ends there"

state = getSentence(
    [["a"]],
    [("ii", (0.0,)), ("p", (0.0,)), ("zza", (0.0,))],
    [("", {"izz": 0.0, "pi": 0.0, "pzz": 1000.0})],
    [])
cost, b = state
assert cost == 3 * wbp
assert b == ["p", "ii", "zza"]
print "It can choose words that increase the length of the leftovers"
