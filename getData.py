import urllib
import os
import zipfile
import math
import sys
from itertools import permutations
from operator import itemgetter

def withoutComment(line):
    commentStart = line.find("//")
    return line if commentStart == -1 else line[:commentStart]

def withoutComments(f):
    for line in f:
        lineWithoutComment = withoutComment(line)
        if not lineWithoutComment.isspace():
            yield lineWithoutComment

def loadShortener(shorteningsPath):
    shortenings = {}
    with open(shorteningsPath, "r") as shorteningsFile:
        for line in withoutComments(shorteningsFile):
            k, v = line.split()
            shortenings[k] = v
    f = lambda x: "".join(shortenings[t] for t in x.split())
    return f

COSTMULTIPLIER = 1000
def showCost(cost):
    return str(int(round(cost * COSTMULTIPLIER)))

def showMenuItem(menuItem):
    value, cost = menuItem
    return value + "=" + str(int(round(cost * COSTMULTIPLIER)))

def showMenu(menu):
    return " ".join(showMenuItem(menuItem) for menuItem in menu)

def generateDeletions(groupsPath, shorteningsPath, deletionsPath):
    shorten = loadShortener(shorteningsPath)

    deletions = {}
    currentCost = 1
    with open(groupsPath, "r") as groupsFile:
        for line in withoutComments(groupsFile):
            try:
                currentCost = float(line.strip())
            except ValueError:
                group = [shorten(i) for i in line.split(",")]
                for key, value in permutations(group, 2):
                    if not value:
                        oldCost = deletions.get(key, float("inf"))
                        deletions[key] = min(oldCost, currentCost)

    deletions = sorted(deletions.iteritems(), key = itemgetter(0))

    with open(deletionsPath, "w") as deletionsFile:
        deletionsFile.writelines(showMenuItem(d) + "\n" for d in deletions)

def generateSubs(groupsPath, shorteningsPath, subsPath):
    shorten = loadShortener(shorteningsPath)

    subs = {}
    currentCost = 1
    with open(groupsPath, "r") as groupsFile:
        for line in withoutComments(groupsFile):
            try:
                currentCost = float(line.strip())
            except ValueError:
                group = [shorten(i) for i in line.split(",")]
                for key, value in permutations(group, 2):
                    if value:
                        menu = subs.setdefault(key, {})
                        oldCost = menu.get(value, float("inf"))
                        menu[value] = min(oldCost, currentCost)

    for key, menu in subs.iteritems():
        subs[key] = sorted(menu.iteritems(), key = itemgetter(0))
    subs = sorted(subs.iteritems(), key = itemgetter(0))

    with open(subsPath, "w") as subsFile:
        subsFile.writelines(k + " " + showMenu(m) + "\n" for k, m in subs)

def parseCMUSpelling(cmuSpelling):
    s = cmuSpelling.strip()
    withoutParens = s.rsplit("(", 1)[0] if s.endswith(")") else s
    return withoutParens.replace("_", " ").lower()

def generatePronouncer(cmuPath, wordsPath, noPronouncePath, shorteningsPath,
                       pronouncerPath):
    shorten = loadShortener(shorteningsPath)

    d = {}
    with open(cmuPath, "r") as cmuFile:
        for line in cmuFile:
            if not line.startswith(";;;"):
                s, p = line.split("\t")
                d.setdefault(parseCMUSpelling(s), []).append(shorten(p))

    with open(noPronouncePath, "r") as noPronounceFile:
        for line in withoutComments(noPronounceFile):
            try:
                s, p = line.split("\t")
            except ValueError:
                del d[parseCMUSpelling(line)]
            else:
                d[parseCMUSpelling(s)].remove(shorten(p))

    with open(wordsPath, "r") as wordsFile:
        for line in withoutComments(wordsFile):
            s, p = line.split("\t")
            d.setdefault(parseCMUSpelling(s), []).append(shorten(p))

    for v in d.itervalues():
        v.sort()
    d = sorted(d.iteritems(), key = itemgetter(0))

    with open(pronouncerPath, "w") as pronouncerFile:
        pronouncerFile.writelines(s + "\t" + " ".join(v) + "\n" for s, v in d)

def generateBook(gbookPath, cmuPath, wordsPath, bookPath):
    spellings = set()
    with open(cmuPath, "r") as cmuFile:
        for line in cmuFile:
            if not line.startswith(";;;"):
                s, _ = line.split("\t")
                spellings.add(parseCMUSpelling(s))

    with open(wordsPath, "r") as wordsFile:
        for line in withoutComments(wordsFile):
            s, _ = line.split("\t")
            spellings.add(parseCMUSpelling(s))

    book = []
    lastCap = None
    lastAdornment = None
    lastCount = 0
    with zipfile.ZipFile(gbookPath, "r") as gbookZipFile:
        gbookFile = gbookZipFile.open(gbookZipFile.namelist()[0], "r")
        for line in gbookFile:
            cap, _, countString, _, _ = line.split("\t")
            if cap != lastCap:
                if lastAdornment != None:
                    book.append((lastAdornment, lastCount))
                lastCap = cap
                lastAdornment = None
                for adornment in cap, "'" + cap, cap + "'", cap + ".":
                    if adornment.lower() in spellings:
                        lastAdornment = adornment
                        break
                lastCount = 0
            lastCount += int(countString)
    if lastAdornment != None:
        book.append((lastAdornment, lastCount))

    with open(bookPath, "w") as bookFile:
        bookFile.writelines(a + "\t" + str(c) + "\n" for a, c in book)

CAPITALMULTIPLIER = 0.1
def adjustCount(count, cap):
    return count * (1 if cap.islower() else CAPITALMULTIPLIER)

COUNTOFFSET = 1
def generateSpeller(pronouncerPath, totalPath, noSpellPath, shorteningsPath,
                    *bookPaths):
    spellerPath = bookPaths[-1]
    bookPaths = bookPaths[:-1]

    shorten = loadShortener(shorteningsPath)

    counts = {}
    for bookPath in bookPaths:
        with open(bookPath, "r") as bookFile:
            for line in withoutComments(bookFile):
                cap, countString = line.split("\t")
                menu = counts.setdefault(cap.lower(), {})
                assert cap not in menu
                menu[cap] = float(countString)

    total = 0.0
    with open(totalPath, "r") as totalFile:
        next(totalFile)
        for line in totalFile:
            _, subTotal, _, _ = line.split("\t")
            total += float(subTotal)
    total += sum(adjustCount(n, cap) - n \
                 for m in counts.itervalues() for cap, n in m.iteritems())

    pronouncer = {}
    with open(pronouncerPath, "r") as pronouncerFile:
        for line in withoutComments(pronouncerFile):
            s, v = line.split("\t")
            pronouncer[s] = v.split()

    with open(noSpellPath, "r") as noSpellFile:
        for line in withoutComments(noSpellFile):
            try:
                s, p = line.split("\t")
            except ValueError:
                del pronouncer[parseCMUSpelling(line)]
            else:
                pronouncer[parseCMUSpelling(s)].remove(shorten(p))

    speller = {}
    for s, v in pronouncer.iteritems():
        for p in v:
            menu = speller.setdefault(p, {})
            menu.update(counts.get(s, {s: 0.0}))
    del pronouncer
    del counts

    for p, menu in speller.iteritems():
        adjustedMenu = \
            ((cap, adjustCount(n, cap)) for cap, n in menu.iteritems())
        bestCap, bestCount = max(adjustedMenu, key = itemgetter(1))
        entropy = -math.log((bestCount + COUNTOFFSET) / total)
        speller[p] = bestCap, entropy

    speller = sorted(speller.iteritems(), key = itemgetter(0))

    with open(spellerPath, "w") as spellerFile:
        spellerFile.writelines(p + "\t" + s + "\t" + showCost(e) + "\n" \
                               for p, (s, e) in speller)

def fileExists(path):
    if os.path.isdir(path):
        print "unexpected dir", path
        sys.exit(1)
    return os.path.isfile(path)

def assertFileExists(path):
    if not fileExists(path):
        print "missing file", path
        sys.exit(1)

def invalidateFile(path):
    if fileExists(path):
        os.remove(path)

def dirExists(path):
    if os.path.isfile(path):
        print "unexpected file", path
        sys.exit(1)
    return os.path.isdir(path)

def ensureDirExists(path):
    dirPath = os.path.dirname(path)
    if not dirExists(dirPath):
        os.makedirs(dirPath)

def ifNotExists(outputPath, f, *inputPaths, **kwargs):
    invalidating = kwargs.get("invalidating", [])
    if not fileExists(outputPath):
        print "generating", os.path.basename(outputPath)
        for inputPath in inputPaths:
            if not inputPath.startswith("http:"):
                assertFileExists(inputPath)
        ensureDirExists(outputPath)
        f(*(inputPaths + (outputPath,)))
        print "\tdone"
        for downstreamPath in invalidating:
            invalidateFile(downstreamPath)
    else:
        print "found", os.path.basename(outputPath)

shortenings = os.path.join(os.getcwd(), "handcraft", "shortenings.txt")
groups = os.path.join(os.getcwd(), "handcraft", "groups.txt")
words = os.path.join(os.getcwd(), "handcraft", "words.txt")
noPronounce = os.path.join(os.getcwd(), "handcraft", "noPronounce.txt")
noSpell = os.path.join(os.getcwd(), "handcraft", "noSpell.txt")

cmuURL = "http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/sphinxdict/cmudict_SPHINX_40"
cmu = os.path.join(os.getcwd(), "cache", "cmudict_SPHINX_40")
totalURL = "http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-us-all-totalcounts-20090715.txt"
total = os.path.join(os.getcwd(), "cache",
                     "googlebooks-eng-us-all-totalcounts-20090715.txt")
gbookURLPrefix = "http://storage.googleapis.com/books/ngrams/books/"
gbooks = [os.path.join(os.getcwd(), "cache",
                       "googlebooks-eng-us-all-1gram-20090715-" + str(i) + \
                           ".csv.zip") \
          for i in xrange(10)]
books = [os.path.join(os.getcwd(), "cache", "book" + str(i) + ".txt") \
         for i in xrange(10)]
counts = os.path.join(os.getcwd(), "cache", "counts.txt")

deletions = os.path.join(os.getcwd(), "data", "deletions.txt")
subs = os.path.join(os.getcwd(), "data", "substitutions.txt")
pronouncer = os.path.join(os.getcwd(), "data", "pronouncer.txt")
speller = os.path.join(os.getcwd(), "data", "speller.txt")

ifNotExists(deletions, generateDeletions, groups, shortenings)
ifNotExists(subs, generateSubs, groups, shortenings)
ifNotExists(cmu, urllib.urlretrieve, cmuURL,
            invalidating = [pronouncer] + books)
ifNotExists(pronouncer, generatePronouncer, cmu, words, noPronounce,
            shortenings, invalidating = [speller])
ifNotExists(total, urllib.urlretrieve, totalURL, invalidating = [speller])
for gbook, book in zip(gbooks, books):
    gbookURL = gbookURLPrefix + os.path.basename(gbook)
    ifNotExists(gbook, urllib.urlretrieve, gbookURL, invalidating = [book])
for gbook, book in zip(gbooks, books):
    ifNotExists(book, generateBook, gbook, cmu, words,
                invalidating = [speller])
ifNotExists(speller, generateSpeller, pronouncer, total, noSpell, shortenings,
            *books)
