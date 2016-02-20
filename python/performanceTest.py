from repronounce import rePronounce, loadDeletions, loadSubs, loadWords
from timeit import timeit
from functools import partial

deletionCosts = loadDeletions("../data/deletions.txt")
subCosts = loadSubs("../data/substitutions.txt")
wordCosts, pronouncer, speller = loadWords("../data/entropyAndPronounce.txt")

challenges = ["Tool Group is the owner of a collection of brands all related to the outdoors and various types of transportation solutions",
              "Tool Group is the owner of a collection of brands all related to the outdoors and various types of transportation solutions It is the market leader in cargo carriers for automobiles and a leading company in the outdoor and bags market with four thousand seven hundred point of sales in one hundred thirty six countries worldwide The Tool motto is Bring your life",
              "Tool Group is the owner of a collection of brands all related to the outdoors and various types of transportation solutions It is the market leader in cargo carriers for automobiles and a leading company in the outdoor and bags market with four thousand seven hundred point of sales in one hundred thirty six countries worldwide The Tool motto is Bring your life Tool was founded in nineteen forty two by the Tooling family in Hill Stop southern Sweden"]
for challenge in challenges:
    s = [pronouncer.get(i) for i in challenge.lower().split()]

    totalTime = timeit(partial(rePronounce, deletionCosts, subCosts, wordCosts, s),
                       number = 1)
    print "words:", len(challenge.split())
    print "time per word:", totalTime / len(challenge.split())
