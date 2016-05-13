import urllib
import os
import zipfile
import math
import sys
from itertools import permutations

def withoutComment(line):
    commentStart = line.find("//")
    return line if commentStart == -1 else line[:commentStart]

def withoutComments(f):
    for line in f:
        lineWithoutComment = withoutComment(line)
        if not lineWithoutComment.isspace():
            yield lineWithoutComment

customDir = os.path.join(os.getcwd(), "customdata")
if not os.path.exists(customDir):
    print "could not find", customDir
    sys.exit(1)
    
shorteningPath = os.path.join(customDir, "phonemeShortenings.txt")    
groupsPath = os.path.join(customDir, "substitutionGroups.txt")
customWordsPath = os.path.join(customDir, "customWords.txt")
removalPath = os.path.join(customDir, "tokensToRemove.txt")

prerequisitePaths = [shorteningPath, groupsPath, customWordsPath, removalPath]
for path in prerequisitePaths:
    if not os.path.isfile(path):
        print "could not find", path
        sys.exit(1)

intermediateDir = os.path.join(os.getcwd(), "intermediate")
if not os.path.exists(intermediateDir):
    os.makedirs(intermediateDir)

outDir = os.path.join(os.path.dirname(os.getcwd()), "data")
if not os.path.exists(outDir):
    os.makedirs(outDir)
    
print "loading phoneme shortenings"
phonemeShortenings = {}
with open(shorteningPath, "r") as shorteningFile:
    for line in withoutComments(shorteningFile):
        tokens = line.split()
        phonemeShortenings[tokens[0]] = tokens[1]

def toPhonemes(tokens):
    return "".join(phonemeShortenings[t] for t in tokens)

RULECOSTMULTIPLIER = 1000

def showValueCost(valueChoice):
    value, cost = valueChoice
    intCost = int(round(cost * RULECOSTMULTIPLIER))
    return "=".join((value, str(intCost)))

substitutionPath = os.path.join(outDir, "substitutions.txt")
if os.path.isfile(substitutionPath):
    print "found substitution rules"
else:
    print "loading substitution groups"
    ruleDict = {}
    currentCost = 1
    with open(groupsPath, "r") as groupsFile:
        for line in withoutComments(groupsFile):
            try:
                currentCost = float(line.strip())
            except ValueError:
                group = []
                for i in line.split(","):
                    group.append(toPhonemes(i.split()))
                for key, value in permutations(group, 2):
                    if value:
                        valueChoices = ruleDict.setdefault(key, {})
                        oldCost = valueChoices.get(value, float("inf"))
                        valueChoices[value] = min(oldCost, currentCost)
    rules = [(key, vcs.items()) for key, vcs in ruleDict.iteritems()]
    del ruleDict
    rules.sort(key = lambda t: t[0])
    for _, valueChoices in rules:
        valueChoices.sort(key = lambda t: t[0])
    
    print "saving substitution rules"
    with open(substitutionPath, "w") as substitutionFile:
        for key, vcs in rules:
            menu = " ".join(showValueCost(v) for v in vcs)
            line = " ".join((key, menu)) + "\n"
            substitutionFile.write(line)

deletionPath = os.path.join(outDir, "deletions.txt")
if os.path.isfile(deletionPath):
    print "found deletion rules"
else:
    print "loading substitution groups"
    deletionDict = {}
    currentCost = 1
    with open(groupsPath, "r") as groupsFile:
        for line in withoutComments(groupsFile):
            try:
                currentCost = float(line.strip())
            except ValueError:
                group = []
                for i in line.split(","):
                    group.append(toPhonemes(i.split()))
                for key, value in permutations(group, 2):
                    if not value:
                        oldCost = deletionDict.get(key, float("inf"))
                        deletionDict[key] = min(oldCost, currentCost)
    deletions = deletionDict.items()
    del deletionDict
    deletions.sort(key = lambda t: t[0])
    
    print "saving deletion rules"
    with open(deletionPath, "w") as deletionFile:
        for deletion in deletions:
            line = showValueCost(deletion) + "\n"
            deletionFile.write(line)

downloadDir = os.path.join(os.getcwd(), "downloads")
if not os.path.exists(downloadDir):
    os.makedirs(downloadDir)

pronounceFilename = "cmudict_SPHINX_40"
pronouncePath = os.path.join(downloadDir, pronounceFilename)
pronounceBaseURL = \
    "http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/sphinxdict/"

totalCountFilename = "googlebooks-eng-us-all-totalcounts-20090715.txt"
totalCountPath = os.path.join(downloadDir, totalCountFilename)
countFilenames = ["googlebooks-eng-us-all-1gram-20090715-" + str(i) + \
                  ".csv.zip" for i in xrange(10)]
countPaths = [os.path.join(downloadDir, f) for f in countFilenames]
countBaseURL = "http://storage.googleapis.com/books/ngrams/books/"

filesToGet = [(pronouncePath, pronounceBaseURL + pronounceFilename),
              (totalCountPath, countBaseURL + totalCountFilename)] \
             + [(os.path.join(downloadDir, name), countBaseURL + name) \
                for name in countFilenames]

for path, url in filesToGet:
    if os.path.isfile(path):
        print "found", os.path.basename(path)
    else:
        print "downloading", os.path.basename(path)
        urllib.urlretrieve(url, path)
print "finished downloading files"

totalCountOutPath = os.path.join(intermediateDir, "totalCount.txt")
if os.path.isfile(totalCountOutPath):
    print "found total count"
else:
    print "generating total count"
    totalCount = 0
    with open(totalCountPath, "r") as totalCountFile:
        next(totalCountFile)
        for line in totalCountFile:
            tokens = line.split("\t")
            totalCount += int(tokens[1])

    print "saving total count"
    with open(totalCountOutPath, "w") as totalCountOutFile:
        totalCountOutFile.write(str(totalCount) + "\n")
    print "saved total count"

CAPITALMULTIPLIER = 0.5

countAndPronouncePath = os.path.join(intermediateDir, "countAndPronounce.txt")
if os.path.isfile(countAndPronouncePath):
    print "found counted pronunciations"
else:
    print "loading tokens to remove"
    tokensToRemove = set()
    with open(removalPath, "r") as removalFile:
        for line in withoutComments(removalFile):
            tokensToRemove.add(line.rstrip("\n"))

    def toSpelling(token):
        allCaps = token.rsplit("(", 1)[0] if token.endswith(")") else token
        spelling = allCaps.replace("_", " ").lower()
        return spelling
    
    print "loading pronunciations"
    pronouncerDict = {}
    with open(pronouncePath, "r") as pronounceFile:
        for line in pronounceFile:
            if not line.startswith(";;;"):
                tokens = line.split()
                if tokens[0] not in tokensToRemove:
                    spelling = toSpelling(tokens[0])
                    phonemes = toPhonemes(tokens[1:])
                    count, ws = pronouncerDict.setdefault(spelling,
                                                          ({spelling: 0}, []))
                    ws.append(phonemes)

    print "loading custom words"
    with open(customWordsPath, "r") as customWordsFile:
        for line in withoutComments(customWordsFile):
            tokens = line.split()
            spelling = toSpelling(tokens[0])
            phonemes = toPhonemes(tokens[1:])
            count, ws = pronouncerDict.setdefault(spelling,
                                                  ({spelling: 0}, []))
            ws.append(phonemes)

    for countPath in countPaths:
        print "loading", os.path.basename(countPath)
        with zipfile.ZipFile(countPath, "r") as countZipFile:
            countFile = countZipFile.open(countZipFile.namelist()[0], "r")
            for line in countFile:
                tokens = line.split("\t")
                basicCap = tokens[0]
                basicSpelling = basicCap.lower()
                adornments = [(basicCap, basicSpelling),
                              ("'" + basicCap, "'" + basicSpelling),
                              (basicCap + "'", basicSpelling + "'"),
                              (basicCap + ".", basicSpelling + ".")]
                for cap, spelling in adornments:
                    try:
                        caps, ws = pronouncerDict[spelling]
                    except KeyError:
                        continue
                    subCount = int(tokens[2])
                    count = caps.get(cap, 0)
                    caps[cap] = count + subCount
                    break
    pronouncer = pronouncerDict.items()
    del pronouncerDict
    pronouncer.sort(key = lambda t: t[0])
    print "finished loading counts"

    print "saving counted pronunciations"
    with open(countAndPronouncePath, "w") as countAndPronounceFile:
        for key, (caps, ws) in pronouncer:
            weightedCount = \
                lambda t: t[1] * (1 if t[0].islower() else CAPITALMULTIPLIER)
            cap, count = max(caps.iteritems(), key = weightedCount)
            for phonemes in ws:
                line = "\t".join((cap, str(count), phonemes)) + "\n"
                countAndPronounceFile.write(line)
    del pronouncer
    print "saved counted pronunciations"

COUNTOFFSET = 1
ENTROPYMULTIPLIER = 1000

entropyAndPronouncePath = os.path.join(outDir, "entropyAndPronounce.txt")
if os.path.isfile(entropyAndPronouncePath):
    print "found pronounciations with entropy"
else:
    print "loading total count"
    with open(totalCountOutPath, "r") as totalCountOutFile:
        totalCount = int(totalCountOutFile.next().rstrip("\n"))
    
    print "loading counted pronunciations"
    pronouncer = []
    totalCapitalCount = 0
    with open(countAndPronouncePath, "r") as countAndPronounceFile:
        for line in countAndPronounceFile:
            tokens = line.rstrip("\n").split("\t")
            cap, strCount, phonemes = tokens
            count = float(strCount)
            if cap.islower():
                totalCapitalCount += count
                count *= CAPITALMULTIPLIER
            if pronouncer and cap == pronouncer[-1][0]:
                pronouncer[-1][2].append(phonemes)
            else:
                pronouncer.append((cap, count, [phonemes]))

    print "calculating entropy"
    totalCount = float(
        totalCount + COUNTOFFSET * len(pronouncer) - \
            (1 - CAPITALMULTIPLIER) * totalCapitalCount)
    for i, (cap, count, ws) in enumerate(pronouncer):
        entropy = -math.log((count + COUNTOFFSET) / totalCount)
        intEntropy = int(round(entropy * ENTROPYMULTIPLIER))
        pronouncer[i] = cap, intEntropy, ws
    
    print "saving pronunciations with entropy"
    with open(entropyAndPronouncePath, "w") as entropyAndPronounceFile:
        for cap, intEntropy, ws in pronouncer:
            for phonemes in ws:
                line = "\t".join((cap, str(intEntropy), phonemes)) + "\n"
                entropyAndPronounceFile.write(line)
    print "saved pronunciations with entropy"
