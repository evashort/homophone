from Queue import PriorityQueue, LifoQueue
from functools import partial
import bisect

WBP = 2.0
SWBP = 1.0

def getToken(s, start):
    aEnd = next((i for i in xrange(start, len(s)) if not s[i].isalpha()),
                len(s))
    dEnd = next((i for i in xrange(start, len(s)) if not s[i].isdigit()),
                len(s))
    end = max(aEnd, dEnd, start + 1)
    return s[start:end]

def getTokens(s):
    start = 0
    while start < len(s):
        token = getToken(s, start)
        yield token
        start += len(token)

def pronounce(pronouncer, sentence):
    tokens = list(getTokens(sentence.lower()))
    start = 0
    while start < len(tokens):
        end = start + 1
        spelling = tokens[start]
        while end < len(tokens) and pronouncer.startwith(spelling):
            end += 1
            spelling = "".join(tokens[start:end])
        ws = pronouncer.get(spelling)
        while ws is None and end > start + 1:
            end -= 1
            spelling = "".join(tokens[start:end])
            ws = pronouncer.get(spelling)
        if ws is not None:
            yield ws
        start = end

def s(sentence):
    s = list(pronounce(pronouncer, sentence))
    solution = rePronounce(deletionCosts, subCosts, wordCosts, s)
    if solution is not None:
        cost, words = solution
        spellings = [speller.get(word) for word in words]
        return cost, spellings

def rePronounce(deletionCosts, subCosts, wordCosts, s):
    dag = getDag(s)
    successorFunc = partial(getStateSuccessors, deletionCosts, subCosts,
                            wordCosts, dag)
    seed = getInitialStates(deletionCosts, dag)
    subSolutions = getKnapsacks(getStateID, getStateKey, successorFunc, seed)
    solution = subSolutions.get(getOverallStateID(dag))
    if solution is not None:
        cost, (words, _) = solution
        return cost, words

def getInitialStates(deletionCosts, dag):
    dagState = 0, True
    for sState, cost in getDeletions(deletionCosts, dag, dagState):
        puzzle = sState, "", ()
        state = [], puzzle
        yield cost, state

def getDag(s):
    dag = []
    fringe = PriorityQueue()
    leftovers = []
    for initialPathList in s:
        initialPaths = frozenset(initialPathList)
        familyLists = {initialPaths: leftovers}
        fringe.put((pathSetKey(initialPaths), initialPaths))
        leftovers = []
        while not fringe.empty():
            _, paths = fringe.get()
            familyList = familyLists[paths]
            for phoneme, siblings in familyList:
                siblings.append((phoneme, len(dag)))
            children = []
            dag.append(children)
            pathLists = {}
            for path in paths:
                if len(path) > 1:
                    pathLists.setdefault(path[0], []).append(path[1:])
                else:
                    leftovers.append((path + " ", children))
            for phoneme, pathList in pathLists.iteritems():
                childPaths = frozenset(pathList)
                try:
                    childFamilyList = familyLists[childPaths]
                except KeyError:
                    familyLists[childPaths] = [(phoneme, children)]
                    fringe.put((pathSetKey(childPaths), childPaths))
                else:
                    childFamilyList.append((phoneme, children))
    for phoneme, siblings in leftovers:
        siblings.append((phoneme, len(dag)))
    dag.append([])
    return dag

def pathSetKey(pathSet):
    return -max(len(path) for path in pathSet)

def getStateID(state):
    _, ((dagIndex, cut), phonemes, spaceCosts) = state
    stateID = dagIndex, cut, phonemes, spaceCosts
    return stateID

def getOverallStateID(dag):
    stateID = len(dag) - 1, True, "", ()
    return stateID

def getStateKey(stateID):
    dagIndex, _, _, _ = stateID
    return dagIndex

def getStateSuccessors(deletionCosts, subCosts, wordCosts, dag, state):
    words, puzzle = state
    for wordCost, word, newPuzzle \
        in getWordChoices(deletionCosts, subCosts, wordCosts, dag, puzzle):
        newWords = words + [word]
        newState = newWords, newPuzzle
        yield wordCost, newState

# a knapsack's successors must not have smaller keys
def getKnapsacks(idFunc, keyFunc, successorFunc, seed):
    knapsacks = {}
    fringe = PriorityQueue()
    for cost, knapsack in seed:
        knapsackID = idFunc(knapsack)
        try:
            oldCost, _ = knapsacks[knapsackID]
        except KeyError:
            knapsacks[knapsackID] = cost, knapsack
            fringe.put((keyFunc(knapsackID), knapsackID))
        else:
            if cost < oldCost:
                knapsacks[knapsackID] = cost, knapsack
    while not fringe.empty():
        knapsackKey, knapsackID = fringe.get()
        cost, knapsack = knapsacks[knapsackID]
        for itemCost, newKnapsack in successorFunc(knapsack):
            newCost = cost + itemCost
            newKnapsackID = idFunc(newKnapsack)
            try:
                oldCost, oldKnapsack = knapsacks[newKnapsackID]
            except KeyError:
                knapsacks[newKnapsackID] = newCost, newKnapsack
                fringe.put((keyFunc(newKnapsackID), newKnapsackID))
            else:
                if newCost < oldCost:
                    knapsacks[newKnapsackID] = newCost, newKnapsack
                    newKnapsackKey = keyFunc(newKnapsackID)
                    if newKnapsackKey == knapsackKey:
                        fringe.put((newKnapsackKey, newKnapsackID))
    return knapsacks

def getSentence(s, w, r, d):
    words = [word for word, _ in w]
    wCosts = [cost[0] for _, cost in w]
    wordCosts = CompletionDict(words, wCosts)
    rKeys = [key for key, _ in r]
    rMenus = [menu.items() for _, menu in r]
    subCosts = CompletionDict(rKeys, rMenus)
    dKeys = [key for key, _ in d]
    dCosts = [cost[""] for _, cost in d]
    deletionCosts = CompletionDict(dKeys, dCosts)
    return rePronounce(deletionCosts, subCosts, wordCosts, s)

def getWordChoices(deletionCosts, subCosts, wordCosts, dag, puzzle):
    fringe = LifoQueue()
    fringe.put((0.0,) + puzzle)
    while not fringe.empty():
        cost, dagState, phonemes, spaceCosts = fringe.get()
        for i, spaceCost in enumerate(spaceCosts):
            splitIndex = i + 1 + len(phonemes) - len(spaceCosts)
            word = phonemes[:splitIndex]
            wordCost = wordCosts.get(word)
            if wordCost is not None:
                newCost = cost + spaceCost + wordCost
                newPuzzle = dagState, phonemes[splitIndex:], spaceCosts[i + 1:]
                wordChoice = newCost, word, newPuzzle
                yield wordChoice
        if wordCosts.startwith(phonemes):
            for sub in getSubChoices(deletionCosts, subCosts, dag, dagState):
                subCost, newDagState, subPhonemes, newSpaceCosts = sub
                newCost = cost + subCost
                newPhonemes = phonemes + subPhonemes
                successor = newCost, newDagState, newPhonemes, newSpaceCosts
                fringe.put(successor)

def getSubChoices(deletionCosts, subCosts, dag, dagState):
    intitalDagIndex, cut = dagState
    initialSpacedKey = ["", ""] if cut else [""]
    fringe = LifoQueue()
    fringe.put((intitalDagIndex, initialSpacedKey))
    while not fringe.empty():
        dagIndex, spacedKey = fringe.get()
        key = "".join(spacedKey)
        cutBeforeDeletions = not spacedKey[-1] if key else cut
        baseDagState = dagIndex, cutBeforeDeletions
        deletions = list(getDeletions(deletionCosts, dag, baseDagState)) \
                    if key else [(baseDagState, 0.0)]
        cutSpacedKey = spacedKey + [""] if spacedKey[-1] else spacedKey
        for value, subCost in getSubCosts(subCosts, key):
            uncutSpaceCosts = getSpaceCosts(spacedKey, value)
            cutSpaceCosts = getSpaceCosts(cutSpacedKey, value)
            for newDagState, deletionCost in deletions:
                newCost = subCost + deletionCost
                _, newCut = newDagState
                spaceCosts = cutSpaceCosts if newCut else uncutSpaceCosts
                subChoice = newCost, newDagState, value, spaceCosts
                yield subChoice
        if subsStartWith(subCosts, key):
            for spacedPhoneme, newDagIndex in dag[dagIndex]:
                phoneme = spacedPhoneme[0]
                space = len(spacedPhoneme) > 1
                newSpacedKey = spacedKey[:-1]
                newSpacedKey.append(spacedKey[-1] + phoneme)
                if space:
                    newSpacedKey.append("")
                successor = newDagIndex, newSpacedKey
                fringe.put(successor)

def getSubCosts(subCosts, key):
    baseSubCosts = subCosts.get(key, [])
    if len(key) == 1:
        return baseSubCosts + [(key, 0.0)]
    else:
        return baseSubCosts

def subsStartWith(subCosts, key):
    return not key or subCosts.startwith(key)
    
def getDeletions(deletions, dag, dagState):
    successorFunc = partial(getDeletionChoices, deletions, dag)
    subSolutions = getKnapsacks(lambda t: t, lambda t: t[0], successorFunc,
                                [(0.0, dagState)])
    for cost, newDagState in subSolutions.itervalues():
        yield newDagState, cost

def getDeletionChoices(deletions, dag, dagState):
    fringe = LifoQueue()
    fringe.put(("", dagState))
    while not fringe.empty():
        key, dagState = fringe.get()
        cost = deletions.get(key)
        if cost is not None:
            yield cost, dagState
        if deletions.startwith(key):
            dagIndex, cut = dagState
            for spacedPhoneme, newDagIndex in dag[dagIndex]:
                phoneme = spacedPhoneme[0]
                space = len(spacedPhoneme) > 1
                newKey = key + phoneme
                newDagState = newDagIndex, cut or space
                fringe.put((newKey, newDagState))

def getSpaceCosts(spacedKey, value):
    if len(spacedKey) == 1:
        return (0.0,) * len(value)
    elif "".join(spacedKey):
        boundary = (0.0,) if spacedKey[-1] else (2.0,)
        startLen = max(len(spacedKey[0]) - 1, 0)
        endLen = max(len(spacedKey[-1]) - 1, 0)
        grayLen = len(value) - startLen - endLen - 1
        if grayLen > 0:
            spaceCosts = (0.0,) * startLen + (1.0,) * grayLen \
                         + (0.0,) * endLen + boundary
        else:
            spaceCosts = (0.0,) * (len(value) - 1) + boundary
    else:
        spaceCosts = (2.0,) * len(value)
    return spaceCosts

class CompletionDict:
    def __init__(self, keys, values):
        assert len(keys) == len(values)
        assert all(keys[i] < keys[i + 1] for i in xrange(len(keys) - 1))
        self.keys = keys
        self.values = values
    def startwith(self, key):
        i = bisect.bisect_right(self.keys, key)
        return i < len(self.keys) and self.keys[i].startswith(key)
    def get(self, key, default = None):
        i = bisect.bisect_left(self.keys, key)
        found = i < len(self.keys) and self.keys[i] == key
        return self.values[i] if found else default

def loadDeletions(filename):
    keys = []
    costs = []
    with open(filename, "r") as f:
        for line in f:
            key, strCost = line.rstrip("\n").split("=")
            keys.append(key)
            costs.append(toCost(strCost))
    deletionCosts = CompletionDict(keys, costs)
    return deletionCosts

def loadSubs(filename):
    keys = []
    menus = []
    with open(filename, "r") as f:
        for line in f:
            tokens = iter(line.rstrip("\n").split(" "))
            key = tokens.next()
            keys.append(key)
            strMenu = (token.split("=") for token in tokens)
            menu = [(value, toCost(strCost)) for value, strCost in strMenu]
            menus.append(menu)
    subCosts = CompletionDict(keys, menus)
    return subCosts

def toCost(strCost):
    return float(strCost) * 0.001

def loadWords(filename):
    keySpellings = []
    wordSets = []
    menu = []
    with open(filename, "r") as f:
        for line in f:
            spelling, strCost, word = line.rstrip("\n").split("\t")
            keySpelling = spelling.lower()
            if not keySpellings or keySpellings[-1] != keySpelling:
                keySpellings.append(keySpelling)
                wordSets.append([])
            wordSets[-1].append(word)
            menu.append((word, wordCost(strCost), spelling))
    pronouncer = CompletionDict(keySpellings, wordSets)
    menu.sort(key = lambda t: t[:2])
    words = []
    costs = []
    spellings = []
    for word, cost, spelling in menu:
        if not words or words[-1] != word:
            words.append(word)
            costs.append(cost)
            spellings.append(spelling)
    wordCosts = CompletionDict(words, costs)
    speller = CompletionDict(words, spellings)
    return wordCosts, pronouncer, speller

def wordCost(strCost):
    return 3 * float(strCost) / 25784

if __name__ == "__main__":
    deletionCosts = loadDeletions("../data/deletions.txt")
    subCosts = loadSubs("../data/substitutions.txt")
    wordCosts, pronouncer, speller = loadWords("../data/entropyAndPronounce.txt")
