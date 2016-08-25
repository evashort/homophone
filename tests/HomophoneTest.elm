module HomophoneTest exposing (homophoneTest)

import ElmTest exposing (..)
import List
import String

import CompletionDict
import BoundaryState exposing (sameSpaceCost, sameWordCost)
import Homophone exposing (Homophone, adultWordLen)

homophoneTest : Test
homophoneTest =
  suite "Homophone Suite"
    [ test
        "It can repeat the same rabbit in order to create the right leftovers" <|
        assertEqual
          (Just ["abca", "bcab", "cabcar"]) <|
          costlessExample
            [["ar"]]
            [("abca", 0.0), ("bcab", 0.0), ("cabcar", 0.0)]
            [("", [("abc", 0.0)])]
            []
    , test
      "It stops repeating the rabbit after trying all possible leftovers" <|
      assertEqual
        Nothing <|
        costlessExample
          [["ar"]]
          [("abca", 0.0), ("bcab", 0.0), ("cabca", 0.0)]
          [("", [("abc", 0.0)])]
          []
    , test
        "it avoids double sameSpaceCost even when the space is cushioned by deletions" <|
        assertEqual
          (Just (["boaton"], t <| 3.9 * sameSpaceCost)) <|
          homophoneExample
            [["bo"], ["ah"], ["up"], ["on"]]
            [("boa", 0.0), ("boah", 0.0), ("boaton", 0.0), ("pon", 0.0), ("upon", 0.0)]
            [("p", [("t", sameSpaceCost * 1.9)])]
            [("h", 0.0), ("u", 0.0)]
    , test
        "It can create multiple words within the same substitution or rabbit" <|
        assertEqual
          (Just ["a", "b", "c", "de", "f", "g"]) <|
          costlessExample
            [["x"]]
            [("a", 0.0), ("b", 0.0), ("c", 0.0), ("de", 0.0), ("f", 0.0), ("g", 0.0)]
            [("", [("efg", 0.0)]), ("x", [("abcd", 0.0)])]
            []
    , test
        "double sameSpaceCost applies in the space left by entire deleted words" <|
        assertEqual
          (Just (["ah", "ha"], t <| 4 * sameSpaceCost + 2 * sameWordCost)) <|
          homophoneExample
            [["a"], ["uu"], ["u"], ["uu"], ["a"]]
            [("ah", 0.0), ("ha", 0.0)]
            [("", [("hh", 0.0)])]
            [("u", 0.0)]
    , test
        "When a word is fully replaced, a boundary within it gets no cost" <|
        assertEqual
          (Just (["ado", "go"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["a"], ["cat"], ["o"]]
            [("ado", 0.0), ("go", 0.0)]
            [("cat", [("dog", 0.0)])]
            []
    , test
        "sameWordCost and double sameSpaceCost apply if key and value both contain space" <|
        assertEqual
          (Just (["bet", "dime"], t <| 2 * sameWordCost + 4 * sameSpaceCost)) <|
          homophoneExample
            [["bed"], ["time"]]
            [("bet", 0.0), ("dime", 0.0)]
            [("dt", [("td", 0.0)])]
            []
    , test
        "It can choose a pronunciation that is completed by the other one" <|
        assertEqual
          (Just (["aha"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["ab", "a"], ["a"]]
            [("abra", 0.0), ("aha", 0.0)]
            [("", [("h", 0.0), ("r", 1.0)])]
            []
    , test
        "It can choose a pronunciation that is a completion of the other one" <|
        assertEqual
          (Just (["abra"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["ab", "a"], ["a"]]
            [("abra", 0.0), ("aha", 0.0)]
            [("", [("h", 1.0), ("r", 0.0)])]
            []
    , test
      "Once it chooses a pronunciation, it can't use parts of the other one" <|
      assertEqual
        Nothing <|
        costlessExample
          [["ab", "cd"]]
          [("ad", 0.0), ("bc", 0.0)]
          []
          []
    , test
        "It can delete even when a substitution makes the word uncontinuable" <|
        assertEqual
          (Just ["c", "d"]) <|
          costlessExample
            [["abh"]]
            [("c", 0.0), ("d", 0.0)]
            [("ab", [("cd", 0.0)])]
            [("h", 0.0)]
    , test
        "Smaller subproblems can overtake larger cheaper ones via a reward" <|
        assertEqual
          (Just (["gif"], t <| sameWordCost + 2 * sameSpaceCost - 1000.0)) <|
          homophoneExample
            [["abc"]]
            [("def", 0.0), ("gif", 0.0)]
            [("a", [("g", 1000.0)]), ("ab", [("de", 0.0)]), ("b", [("i", -2000.0)]), ("c", [("f", 0.0)])]
            []
    , test
        "It stops accumulating reward when there are no more matching words" <|
        assertEqual
          (Just (["hhot"], t <| sameWordCost + 2 * sameSpaceCost - 1999.0)) <|
          homophoneExample
            [["at"]]
            [("hhot", 0.0)]
            [("", [("h", -1000.0)]), ("a", [("o", 1.0)])]
            []
    , test
        "It can choose the more expensive word to avoid a substitution" <|
        assertEqual
          (Just (["cot"], t <| 2.0 * adultWordLen + sameWordCost + 2 * sameSpaceCost)) <|
          homophoneExample
            [["cot"]]
            [("cat", 1.0), ("cot", 2.0)]
            [("o", [("a", 2.0 * adultWordLen)])]
            []
    , test
        "It can make an expensive substitution to avoid the more exensive word" <|
        assertEqual
          (Just (["cat"], t <| 2.0 * adultWordLen + sameWordCost + 2 * sameSpaceCost)) <|
          homophoneExample
            [["cot"]]
            [("cat", 1.0), ("cot", 3.0)]
            [("o", [("a", 1.0 * adultWordLen)])]
            []
    , test
        "It distinguishes puzzles by whether there is a space on the boundary" <|
        -- cost(sa) should be greater than cost(s),
        -- but cost(sa x yc) should be less than cost(s x yc)
        -- cost(sa) = sameSpaceCost + cost(deleting b)
        -- cost(s) = sameWordCost + 2 * sameSpaceCost
        -- cost(sa x yc) = 2 * sameSpaceCost + cost(deleting b)
        -- cost(s x yc) = 2 * sameWordCost + 4 * sameSpaceCost
        -- therefore cost(deleting b) > sameWordCost + sameSpaceCost
        -- and cost(deleting b) < 2 * sameWordCost + 2 * sameSpaceCost
        assertEqual
          (Just (["sa", "x", "yc"], t <| 3.5 * sameSpaceCost + 2 * sameWordCost)) <|
          homophoneExample
            [["s"], ["abc"]]
            [("s", 0.0), ("sa", 0.0), ("x", 0.0), ("yc", 0.0)]
            [("", [("xy", 0.0)])]
            [("ab", 0.0), ("b", 1.5 * sameSpaceCost + 2 * sameWordCost)]
    , test
        "It doesn't prematurely expand puzzles created by deletions alone" <|
        assertEqual
          (Just (["a", "x", "yc"], t <| 4 * sameSpaceCost + 2 * sameWordCost)) <|
          homophoneExample
            [["a"], ["bc"]]
            [("a", 0.0), ("x", 0.0), ("yc", 0.0)]
            [("", [("xy", 0.0)])]
            [("ab", 1000.0), ("b", 0.0)]
    , test
        "It can choose words that increase the length of the leftovers" <|
        assertEqual
          (Just (["p", "ii", "zza"], t <| 2 * sameSpaceCost + sameWordCost)) <|
          homophoneExample
            [["a"]]
            [("ii", 0.0), ("p", 0.0), ("zza", 0.0)]
            [("", [("izz", 0.0), ("pi", 0.0), ("pzz", 1000.0)])]
            []
    , test
        "sameWordCost applies to words ending in spaced 1val + rabbit" <|
        assertEqual
          (Just (["balladh", "in", "ner"], t <| sameWordCost + 3 * sameSpaceCost)) <|
          homophoneExample
            [["ballat"], ["dinner"]]
            [("balladh", 0.0), ("in", 0.0), ("ner", 0.0)]
            [("", [("h", 0.0)]), ("td", [("d", 0.0)])]
            []
    , test
        "sameWordCost n/a if next word starts with spaced 1val" <|
        assertEqual
          (Just (["balla", "tin", "ner"], t <| 3 * sameSpaceCost)) <|
          homophoneExample
            [["ballat"], ["dinner"]]
            [("balla", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("td", [("t", 0.0)])]
            []
    , test
        "sameWordCost n/a if next word starts with rabbit + spaced 1val" <|
        assertEqual
          (Just (["balla", "htin", "ner"], t <| 3 * sameSpaceCost)) <|
          homophoneExample
            [["ballat"], ["dinner"]]
            [("balla", 0.0), ("htin", 0.0), ("ner", 0.0)]
            [("", [("h", 0.0)]), ("td", [("t", 0.0)])]
            []
    , test
        "sameWordCost n/a if next word starts with spaced nval" <|
        assertEqual
          (Just (["balla", "dtin", "ner"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["ballat"], ["dinner"]]
            [("balla", 0.0), ("dtin", 0.0), ("ner", 0.0)]
            [("td", [("dt", 0.0)])]
            []
    , test
        "sameWordCost n/a if next word has real sub before spaced 1val" <|
        assertEqual
          (Just (["ball", "odin", "ner"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["ballat"], ["dinner"]]
            [("ball", 0.0), ("ner", 0.0), ("odin", 0.0)]
            [("a", [("o", 0.0)]), ("td", [("d", 0.0)])]
            []
    , test
        "sameWordCost applies if final nval is from rspaced 1key" <|
        assertEqual
          (Just (["ballat", "tin", "ner"], t <| sameWordCost + 3 * sameSpaceCost)) <|
          homophoneExample
            [["ballat"], ["inner"]]
            [("ballat", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("t", [("tt", 0.0)])]
            []
    , test
        "sameWordCost n/a if final nval is from lspaced 1key" <|
        assertEqual
          (Just (["ballad", "din", "ner"], t <| 3 * sameSpaceCost)) <|
          homophoneExample
            [["balla"], ["dinner"]]
            [("ballad", 0.0), ("din", 0.0), ("ner", 0.0)]
            [("d", [("dd", 0.0)])]
            []
    , test
        "sameWordCost n/a if final sub starts with space" <|
        assertEqual
          (Just (["ballad", "tin", "ner"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["balla"], ["td"], ["inner"]]
            [("ballad", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("td", [("dt", 0.0)])]
            []
    , test
        "sameWordCost applies if word starts with rabbit + spaced 1val " <|
        assertEqual
          (Just (["bal", "la", "htinner"], t <| sameWordCost + 3 * sameSpaceCost)) <|
          homophoneExample
            [["ballat"], ["dinner"]]
            [("bal", 0.0), ("htinner", 0.0), ("la", 0.0)]
            [("", [("h", 0.0)]), ("td", [("t", 0.0)])]
            []
    , test
        "sameWordCost n/a if initial nval is from rspaced 1key" <|
        assertEqual
          (Just (["bal", "lad", "dinner"], t <| 3 * sameSpaceCost)) <|
          homophoneExample
            [["ballad"], ["inner"]]
            [("bal", 0.0), ("dinner", 0.0), ("lad", 0.0)]
            [("d", [("dd", 0.0)])]
            []
    , test
        "sameWordCost n/a if word starts inside unspaced nval" <|
        assertEqual
          (Just (["bal", "lad", "tinner"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["balla"], ["tdinner"]]
            [("bal", 0.0), ("lad", 0.0), ("tinner", 0.0)]
            [("td", [("dt", 0.0)])]
            []
    , test
        "sameWordCost n/a if word starts inside nval from nkey followed by space" <|
        assertEqual
          (Just (["bal", "lat", "dinner"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["balladt"], ["inner"]]
            [("bal", 0.0), ("dinner", 0.0), ("lat", 0.0)]
            [("dt", [("td", 0.0)])]
            []
    , test
        "sameWordCost n/a if word ends inside unspaced nval" <|
        assertEqual
          (Just (["ballad", "tin", "ner"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["ballatd"], ["inner"]]
            [("ballad", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("td", [("dt", 0.0)])]
            []
    , test
        "sameWordCost n/a if final nval is from 1key with deletion after lspace" <|
        assertEqual
          (Just (["ballad", "din", "ner"], t <| 3 * sameSpaceCost)) <|
          homophoneExample
            [["balla"], ["hdinner"]]
            [("ballad", 0.0), ("din", 0.0), ("ner", 0.0)]
            [("d", [("dd", 0.0)])]
            [("h", 0.0)]
    , test
        "sameWordCost applies if final nval is from 1key with deletion before rspace" <|
        assertEqual
          (Just (["ballat", "tin", "ner"], t <| sameWordCost + 3 * sameSpaceCost)) <|
          homophoneExample
            [["ballath"], ["inner"]]
            [("ballat", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("t", [("tt", 0.0)])]
            [("h", 0.0)]
    , test
        "sameWordCost n/a if next word starts with spaced 1val followed by deletion" <|
        assertEqual
          (Just (["balla", "tin", "ner"], t <| 3 * sameSpaceCost)) <|
          homophoneExample
            [["ballat"], ["dhinner"]]
            [("balla", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("td", [("t", 0.0)])]
            [("h", 0.0)]
    , test
        "sameWordCost n/a if next word starts with unspaced 1val from nkey + space + deletion" <|
        assertEqual
          (Just (["balla", "tin", "ner"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["ballatd"], ["hinner"]]
            [("balla", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("td", [("t", 0.0)])]
            [("h", 0.0)]
    , test
        "sameWordCost n/a if word starts inside nval from 1key + space + deletion" <|
        assertEqual
          (Just (["bal", "lat", "tinner"], t <| 3 * sameSpaceCost)) <|
          homophoneExample
            [["ballat"], ["hinner"]]
            [("bal", 0.0), ("lat", 0.0), ("tinner", 0.0)]
            [("t", [("tt", 0.0)])]
            [("h", 0.0)]
    , test
        "sameWordCost n/a if word starts inside nval from 1key + deletion + space" <|
        assertEqual
          (Just (["bal", "lat", "tinner"], t <| 3 * sameSpaceCost)) <|
          homophoneExample
            [["ballath"], ["inner"]]
            [("bal", 0.0), ("lat", 0.0), ("tinner", 0.0)]
            [("t", [("tt", 0.0)])]
            [("h", 0.0)]
    , test
        "sameWordCost n/a if word starts inside unspaced nval from nval + space + deletion" <|
        assertEqual
          (Just (["bal", "lad", "tinner"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["ballatd"], ["hinner"]]
            [("bal", 0.0), ("lad", 0.0), ("tinner", 0.0)]
            [("td", [("dt", 0.0)])]
            [("h", 0.0)]
    , test
        "sameWordCost n/a if word starts inside unspaced nval followed by space" <|
        assertEqual
          (Just (["bal", "lad", "tinner"], t <| 2 * sameSpaceCost)) <|
          homophoneExample
            [["balla"], ["td"], ["inner"]]
            [("bal", 0.0), ("lad", 0.0), ("tinner", 0.0)]
            [("td", [("dt", 0.0)])]
            []
    , test
        "kid word costs are multiplied by adultWordLen" <|
        assertEqual
          (Just (["a"], t <| 5.0 * adultWordLen + sameWordCost + 2 * sameSpaceCost)) <|
          homophoneExample
            [["a"]]
            [("a", 5.0)]
            []
            []
    , test
        "adult word costs are multiplied by the word's length" <|
        let n = truncate adultWordLen + 3 in
          assertEqual
            (Just ([String.repeat n "a"], t <| 5.0 * toFloat n + sameWordCost + 2 * sameSpaceCost)) <|
            homophoneExample
              [[String.repeat n "a"]]
              [(String.repeat n "a", 5.0)]
              []
              []
    , test
        "using just the last of 3 values for a 1-key still avoids sameSpaceCost and sameWordCost" <|
        assertEqual
          (Just (["bur", "rI"], t <| 3.5 * sameSpaceCost + 1.5 * sameWordCost)) <|
          homophoneExample
            [["bR"], ["I"]]
            [("I", 0.0), ("bur", 0.0), ("rI", 0.5 * (sameSpaceCost + sameWordCost) / adultWordLen)]
            [("R", [("ur", 0.0), ("urr", 0.0)])]
            []
    , test
        "using just the first of 3 values for a 1-key still avoids sameSpaceCost and sameWordCost" <|
        assertEqual
          (Just (["Ir", "rub"], t <| 3.5 * sameSpaceCost + 1.5 * sameWordCost)) <|
          homophoneExample
            [["I"], ["Rb"]]
            [("I", 0.0), ("Ir", 0.5 * (sameSpaceCost + sameWordCost) / adultWordLen), ("rub", 0.0)]
            [("R", [("rru", 0.0), ("ru", 0.0)])]
            []
    , test
        "caahe" <|
        assertEqual
          [ Just (["bb"], t <| 2 * sameSpaceCost)
          , Just (["bbb"], t <| 2 * sameSpaceCost)
          ] <|
          cacheExample
            [ [["b"], ["b"]]
            , [["b"], ["b"], ["b"]]
            ]
            [("b", 0.0), ("bb", 0.0), ("bbb", 0.0)]
            []
            []
    , test
        "caahe2" <|
        assertEqual
          [ Just (["be"], t <| 1 * sameWordCost + 2 * sameSpaceCost)
          , Just (["bebe"], t <| 2 * sameSpaceCost)
          , Just (["bib", "ebe"], t <| 2 * sameSpaceCost + 0.1)
          ] <|
          cacheExample
            [ [["be"]]
            , [["be"], ["be"]]
            , [["be"], ["be"], ["be"]]
            ]
            [("be", 0.0), ("bebe", 0.0), ("bib", 0.0), ("ebe", 0.0), ("hub", 0.0), ("up", 0.0)]
            [("", [("h", 0.1), ("u", 0.1)]), ("b", [("p", 0.1)]), ("e", [("i", 0.1)])]
            []
    , test
        "caahe3" <|
        assertEqual
          [ Just (["b"], t <| 1 * sameWordCost + 2 * sameSpaceCost)
          , Just (["b", "b"], t <| 2 * sameWordCost + 4 * sameSpaceCost)
          ] <|
          cacheExample
            [ [["b"]]
            , [["b"], ["b"]]
            ]
            [("b", 0.0), ("ub", 0.0)]
            [("", [("u", 0.1)])]
            []
    ]

cacheExample :
  List (List (List String)) -> List (String, Float) ->
    List (String, List (String, Float)) -> List (String, Float) ->
    List (Maybe (List String, Float))
cacheExample sentences wCosts sCosts dCosts =
  let
    maybeDCosts = CompletionDict.fromSortedPairs dCosts
    maybeSCosts = CompletionDict.fromSortedPairs sCosts
    maybeWCosts = CompletionDict.fromSortedPairs wCosts
  in
    case (maybeDCosts, maybeSCosts, maybeWCosts) of
      (Just dCosts, Just sCosts, Just wCosts) ->
        List.reverse <|
          fst <|
            List.foldl
              cacheExampleHelper
              ([], Homophone.init dCosts sCosts wCosts)
              sentences
      _ -> []

cacheExampleHelper :
  List (List String) -> (List (Maybe (List String, Float)), Homophone) ->
    (List (Maybe (List String, Float)), Homophone)
cacheExampleHelper sentence (statuses, cache) =
  let
    newHomophone = Homophone.update 100 <| Homophone.setGoal sentence cache
  in
    (interpretHomophone newHomophone :: statuses, fst newHomophone)


homophoneExample :
  List (List String) -> List (String, Float) ->
    List (String, List (String, Float)) -> List (String, Float) ->
      Maybe (List String, Float)
homophoneExample sentence wCosts sCosts dCosts =
  let
    maybeDCosts = CompletionDict.fromSortedPairs dCosts
    maybeSCosts = CompletionDict.fromSortedPairs sCosts
    maybeWCosts = CompletionDict.fromSortedPairs wCosts
  in
    case (maybeDCosts, maybeSCosts, maybeWCosts) of
      (Just dCosts, Just sCosts, Just wCosts) ->
        let
          data =
            { dCosts = dCosts
            , sCosts = sCosts
            , wCosts = wCosts
            }
        in
          interpretHomophone <|
            Homophone.update
              100 <|
              Homophone.setGoal
                sentence <|
                Homophone.init dCosts sCosts wCosts
      _ -> Debug.crash "test data not sorted"

costlessExample :
  List (List String) -> List (String, Float) ->
    List (String, List (String, Float)) -> List (String, Float) ->
    Maybe (List String)
costlessExample sentence wCosts sCosts dCosts =
  Maybe.map fst <| homophoneExample sentence wCosts sCosts dCosts

interpretHomophone : (Homophone, Int) -> Maybe (List String, Float)
interpretHomophone (homophone, _) =
  if Homophone.done homophone then
    if Homophone.complete homophone then
      Just (Homophone.pronunciation homophone, t <| Homophone.cost homophone)
    else Nothing
  else Debug.crash "ran out of iterations"

t : Float -> Float -- 2^31, highest power of 2 that doen't give NaN
t x = toFloat (round (x * 2147483648)) / 2147483648
