from repronounce import getDag

def dagToDict(dag):
    d = {}
    for children in dag:
        for phoneme, childIndex in children:
            assert phoneme not in d
            d[phoneme] = set((p for p, _ in dag[childIndex]))
    return d

dag = getDag([["abc", "anz", "xyz"]])
assert dagToDict(dag) == {"a": {"b", "n"},
                          "x": {"y"},
                          "n": {"z "},
                          "y": {"z "},
                          "b": {"c "},
                          "z ": set(),
                          "c ": set()}
print "it can merge redundant branches of the DAG"

print "when a child has two parents and one of them has a shorter path than it"
dag = getDag([["pile", "bale", "po"]])
assert dagToDict(dag) == {"p": {"i", "o "},
                          "o ": set(),
                          "i": {"l"},
                          "b": {"a"},
                          "a": {"l"},
                          "l": {"e "},
                          "e ": set()}
print "\tthe child still gets merged"
