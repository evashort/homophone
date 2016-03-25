module Tests where

import ElmTest exposing (..)
import List
import String

import CompletionDict
import BoundaryState exposing (spaceCost, wordCost)
import Respell

all : Test
all =
  suite "Respell Suite"
    [ test
        "It can repeat the same rabbit in order to create the right leftovers" <|
        assertEqual
          (Just "abca bcab cabcar") <|
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
        "it avoids double spaceCost even when the space is cushioned by deletions" <|
        assertEqual
          (Just ("boaton", 3.9 * spaceCost)) <|
          respellExample
            [["bo"], ["ah"], ["up"], ["on"]]
            [("boa", 0.0), ("boah", 0.0), ("boaton", 0.0), ("pon", 0.0), ("upon", 0.0)]
            [("p", [("t", spaceCost * 1.9)])]
            [("h", 0.0), ("u", 0.0)]
    , test
        "It can create multiple words within the same substitution or rabbit" <|
        assertEqual
          (Just "a b c de f g") <|
          costlessExample
            [["x"]]
            [("a", 0.0), ("b", 0.0), ("c", 0.0), ("de", 0.0), ("f", 0.0), ("g", 0.0)]
            [("", [("efg", 0.0)]), ("x", [("abcd", 0.0)])]
            []
    , test
        "double spaceCost applies in the space left by entire deleted words" <|
        assertEqual
          (Just ("ah ha", 4 * spaceCost + 2 * wordCost)) <|
          respellExample
            [["a"], ["uu"], ["u"], ["uu"], ["a"]]
            [("ah", 0.0), ("ha", 0.0)]
            [("", [("hh", 0.0)])]
            [("u", 0.0)]
    , test
        "When a word is fully replaced, a boundary within it gets no cost" <|
        assertEqual
          (Just ("ado go", 2 * spaceCost)) <|
          respellExample
            [["a"], ["cat"], ["o"]]
            [("ado", 0.0), ("go", 0.0)]
            [("cat", [("dog", 0.0)])]
            []
    , test
        "wordCost and double spaceCost apply if key and value both contain space" <|
        assertEqual
          (Just ("bet dime", 2 * wordCost + 4 * spaceCost)) <|
          respellExample
            [["bed"], ["time"]]
            [("bet", 0.0), ("dime", 0.0)]
            [("dt", [("td", 0.0)])]
            []
    , test
        "It can choose a pronunciation that is completed by the other one" <|
        assertEqual
          (Just ("aha", 2 * spaceCost)) <|
          respellExample
            [["ab", "a"], ["a"]]
            [("abra", 0.0), ("aha", 0.0)]
            [("", [("h", 0.0), ("r", 1.0)])]
            []
    , test
        "It can choose a pronunciation that is a completion of the other one" <|
        assertEqual
          (Just ("abra", 2 * spaceCost)) <|
          respellExample
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
          (Just "c d") <|
          costlessExample
            [["abh"]]
            [("c", 0.0), ("d", 0.0)]
            [("ab", [("cd", 0.0)])]
            [("h", 0.0)]
    , test
        "Smaller subproblems can overtake larger cheaper ones via a reward" <|
        assertEqual
          (Just ("gif", wordCost + 2 * spaceCost - 1000.0)) <|
          respellExample
            [["abc"]]
            [("def", 0.0), ("gif", 0.0)]
            [("a", [("g", 1000.0)]), ("ab", [("de", 0.0)]), ("b", [("i", -2000.0)]), ("c", [("f", 0.0)])]
            []
    , test
        "It stops accumulating reward when there are no more matching words" <|
        assertEqual
          (Just ("hhot", wordCost + 2 * spaceCost - 1999.0)) <|
          respellExample
            [["at"]]
            [("hhot", 0.0)]
            [("", [("h", -1000.0)]), ("a", [("o", 1.0)])]
            []
    , test
        "It can choose the more expensive word to avoid a substitution" <|
        assertEqual
          (Just ("cot", 2.0 + wordCost + 2 * spaceCost)) <|
          respellExample
            [["cot"]]
            [("cat", 1.0), ("cot", 2.0)]
            [("o", [("a", 2.0)])]
            []
    , test
        "It can make an expensive substitution to avoid the more exensive word" <|
        assertEqual
          (Just ("cat", 2.0 + wordCost + 2 * spaceCost)) <|
          respellExample
            [["cot"]]
            [("cat", 1.0), ("cot", 3.0)]
            [("o", [("a", 1.0)])]
            []
    , test
        "It distinguishes puzzles by whether there is a space on the boundary" <|
        assertEqual
          (Just ("sa x yc", 3.5 * spaceCost + 2 * wordCost)) <|
          respellExample
            [["s"], ["abc"]]
            [("s", 0.0), ("sa", 0.0), ("x", 0.0), ("yc", 0.0)]
            [("", [("xy", 0.0)])]
            [("ab", 0.0), ("b", 1.5 * spaceCost + 2 * wordCost)]
    , test
        "It doesn't prematurely expand puzzles created by deletions alone" <|
        assertEqual
          (Just ("a x yc", 4 * spaceCost + 2 * wordCost)) <|
          respellExample
            [["a"], ["bc"]]
            [("a", 0.0), ("x", 0.0), ("yc", 0.0)]
            [("", [("xy", 0.0)])]
            [("ab", 1000.0), ("b", 0.0)]
    , test
        "It can choose words that increase the length of the leftovers" <|
        assertEqual
          (Just ("p ii zza", 2 * spaceCost + wordCost)) <|
          respellExample
            [["a"]]
            [("ii", 0.0), ("p", 0.0), ("zza", 0.0)]
            [("", [("izz", 0.0), ("pi", 0.0), ("pzz", 1000.0)])]
            []
    , test
        "wordCost applies to words ending in spaced 1val + rabbit" <|
        assertEqual
          (Just ("balladh in ner", wordCost + 3 * spaceCost)) <|
          respellExample
            [["ballat"], ["dinner"]]
            [("balladh", 0.0), ("in", 0.0), ("ner", 0.0)]
            [("", [("h", 0.0)]), ("td", [("d", 0.0)])]
            []
    , test
        "wordCost n/a if next word starts with spaced 1val" <|
        assertEqual
          (Just ("balla tin ner", 3 * spaceCost)) <|
          respellExample
            [["ballat"], ["dinner"]]
            [("balla", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("td", [("t", 0.0)])]
            []
    , test
        "wordCost n/a if next word starts with rabbit + spaced 1val" <|
        assertEqual
          (Just ("balla htin ner", 3 * spaceCost)) <|
          respellExample
            [["ballat"], ["dinner"]]
            [("balla", 0.0), ("htin", 0.0), ("ner", 0.0)]
            [("", [("h", 0.0)]), ("td", [("t", 0.0)])]
            []
    , test
        "wordCost n/a if next word starts with spaced nval" <|
        assertEqual
          (Just ("balla dtin ner", 2 * spaceCost)) <|
          respellExample
            [["ballat"], ["dinner"]]
            [("balla", 0.0), ("dtin", 0.0), ("ner", 0.0)]
            [("td", [("dt", 0.0)])]
            []
    , test
        "wordCost n/a if next word has real sub before spaced 1val" <|
        assertEqual
          (Just ("ball odin ner", 2 * spaceCost)) <|
          respellExample
            [["ballat"], ["dinner"]]
            [("ball", 0.0), ("ner", 0.0), ("odin", 0.0)]
            [("a", [("o", 0.0)]), ("td", [("d", 0.0)])]
            []
    , test
        "wordCost applies if final nval is from rspaced 1key" <|
        assertEqual
          (Just ("ballat tin ner", wordCost + 3 * spaceCost)) <|
          respellExample
            [["ballat"], ["inner"]]
            [("ballat", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("t", [("tt", 0.0)])]
            []
    , test
        "wordCost n/a if final nval is from lspaced 1key" <|
        assertEqual
          (Just ("ballad din ner", 3 * spaceCost)) <|
          respellExample
            [["balla"], ["dinner"]]
            [("ballad", 0.0), ("din", 0.0), ("ner", 0.0)]
            [("d", [("dd", 0.0)])]
            []
    , test
        "wordCost n/a if final sub starts with space" <|
        assertEqual
          (Just ("ballad tin ner", 2 * spaceCost)) <|
          respellExample
            [["balla"], ["td"], ["inner"]]
            [("ballad", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("td", [("dt", 0.0)])]
            []
    , test
        "wordCost applies if word starts with rabbit + spaced 1val " <|
        assertEqual
          (Just ("bal la htinner", wordCost + 3 * spaceCost)) <|
          respellExample
            [["ballat"], ["dinner"]]
            [("bal", 0.0), ("htinner", 0.0), ("la", 0.0)]
            [("", [("h", 0.0)]), ("td", [("t", 0.0)])]
            []
    , test
        "wordCost n/a if initial nval is from rspaced 1key" <|
        assertEqual
          (Just ("bal lad dinner", 3 * spaceCost)) <|
          respellExample
            [["ballad"], ["inner"]]
            [("bal", 0.0), ("dinner", 0.0), ("lad", 0.0)]
            [("d", [("dd", 0.0)])]
            []
    , test
        "wordCost n/a if word starts inside unspaced nval" <|
        assertEqual
          (Just ("bal lad tinner", 2 * spaceCost)) <|
          respellExample
            [["balla"], ["tdinner"]]
            [("bal", 0.0), ("lad", 0.0), ("tinner", 0.0)]
            [("td", [("dt", 0.0)])]
            []
    , test
        "wordCost n/a if word starts inside nval from nkey followed by space" <|
        assertEqual
          (Just ("bal lat dinner", 2 * spaceCost)) <|
          respellExample
            [["balladt"], ["inner"]]
            [("bal", 0.0), ("dinner", 0.0), ("lat", 0.0)]
            [("dt", [("td", 0.0)])]
            []
    , test
        "wordCost n/a if word ends inside unspaced nval" <|
        assertEqual
          (Just ("ballad tin ner", 2 * spaceCost)) <|
          respellExample
            [["ballatd"], ["inner"]]
            [("ballad", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("td", [("dt", 0.0)])]
            []
    , test
        "wordCost n/a if final nval is from 1key with deletion after lspace" <|
        assertEqual
          (Just ("ballad din ner", 3 * spaceCost)) <|
          respellExample
            [["balla"], ["hdinner"]]
            [("ballad", 0.0), ("din", 0.0), ("ner", 0.0)]
            [("d", [("dd", 0.0)])]
            [("h", 0.0)]
    , test
        "wordCost applies if final nval is from 1key with deletion before rspace" <|
        assertEqual
          (Just ("ballat tin ner", wordCost + 3 * spaceCost)) <|
          respellExample
            [["ballath"], ["inner"]]
            [("ballat", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("t", [("tt", 0.0)])]
            [("h", 0.0)]
    , test
        "wordCost n/a if next word starts with spaced 1val followed by deletion" <|
        assertEqual
          (Just ("balla tin ner", 3 * spaceCost)) <|
          respellExample
            [["ballat"], ["dhinner"]]
            [("balla", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("td", [("t", 0.0)])]
            [("h", 0.0)]
    , test
        "wordCost n/a if next word starts with unspaced 1val from nkey + space + deletion" <|
        assertEqual
          (Just ("balla tin ner", 2 * spaceCost)) <|
          respellExample
            [["ballatd"], ["hinner"]]
            [("balla", 0.0), ("ner", 0.0), ("tin", 0.0)]
            [("td", [("t", 0.0)])]
            [("h", 0.0)]
    , test
        "wordCost n/a if word starts inside nval from 1key + space + deletion" <|
        assertEqual
          (Just ("bal lat tinner", 3 * spaceCost)) <|
          respellExample
            [["ballat"], ["hinner"]]
            [("bal", 0.0), ("lat", 0.0), ("tinner", 0.0)]
            [("t", [("tt", 0.0)])]
            [("h", 0.0)]
    , test
        "wordCost n/a if word starts inside nval from 1key + deletion + space" <|
        assertEqual
          (Just ("bal lat tinner", 3 * spaceCost)) <|
          respellExample
            [["ballath"], ["inner"]]
            [("bal", 0.0), ("lat", 0.0), ("tinner", 0.0)]
            [("t", [("tt", 0.0)])]
            [("h", 0.0)]
    , test
        "wordCost n/a if word starts inside unspaced nval from nval + space + deletion" <|
        assertEqual
          (Just ("bal lad tinner", 2 * spaceCost)) <|
          respellExample
            [["ballatd"], ["hinner"]]
            [("bal", 0.0), ("lad", 0.0), ("tinner", 0.0)]
            [("td", [("dt", 0.0)])]
            [("h", 0.0)]
    , test
        "wordCost n/a if word starts inside unspaced nval followed by space" <|
        assertEqual
          (Just ("bal lad tinner", 2 * spaceCost)) <|
          respellExample
            [["balla"], ["td"], ["inner"]]
            [("bal", 0.0), ("lad", 0.0), ("tinner", 0.0)]
            [("td", [("dt", 0.0)])]
            []
    , test
        "caahe" <|
        assertEqual
          [ Just ("bb", 2 * spaceCost)
          , Just ("bbb", 2 * spaceCost)
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
          [ Just ("be", 1 * wordCost + 2 * spaceCost)
          , Just ("bebe", 2 * spaceCost)
          , Just ("bib ebe", 2 * spaceCost + 0.1)
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
          [ Just ("b", 1 * wordCost + 2 * spaceCost)
          , Just ("b b", 2 * wordCost + 4 * spaceCost)
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
    List (Maybe (String, Float))
cacheExample sentences wordCosts subCosts deletionCosts =
  let
    maybeDeletionCosts = CompletionDict.fromSortedPairs deletionCosts
    maybeSubCosts = CompletionDict.fromSortedPairs subCosts
    maybeWordCosts =
      CompletionDict.fromSortedPairs <|
        List.map2 (,) (List.map fst wordCosts) wordCosts
  in
    case
      (maybeDeletionCosts, maybeSubCosts, maybeWordCosts)
    of
      (Just deletionCosts, Just subCosts, Just wordCosts) ->
        let
          laundry =
            List.foldl
              ( asdf
                  { pronouncer = CompletionDict.empty
                  , deletionCosts = deletionCosts
                  , subCosts = subCosts
                  , wordCosts = wordCosts
                  }
              )
              ([], Respell.emptyCache)
              sentences
        in
          List.reverse <| fst laundry
      _ -> []

asdf :
  Respell.LoadedData -> List (List String) ->
    (List (Maybe (String, Float)), Respell.Cache) ->
    (List (Maybe (String, Float)), Respell.Cache)
asdf data sentence (respellings, cache) =
  let
    maybePronouncer = -- does not support more than 10 words
        CompletionDict.fromSortedPairs <|
          List.indexedMap ((,) << toString) sentence
  in
    case maybePronouncer of
      Nothing -> (Nothing :: respellings, Respell.emptyCache)
      Just pronouncer ->
        let
          result =
            Respell.respell
              { data | pronouncer = pronouncer }
              cache <|
              String.join " " <| List.indexedMap (always << toString) sentence
        in
          (result.respelling :: respellings, result.cache)


respellExample :
  List (List String) -> List (String, Float) ->
    List (String, List (String, Float)) -> List (String, Float) ->
    Maybe (String, Float)
respellExample sentence wordCosts subCosts deletionCosts =
  let
    maybePronouncer = -- does not support more than 10 words
        CompletionDict.fromSortedPairs <|
          List.indexedMap ((,) << toString) sentence
    maybeDeletionCosts = CompletionDict.fromSortedPairs deletionCosts
    maybeSubCosts = CompletionDict.fromSortedPairs subCosts
    maybeWordCosts =
      CompletionDict.fromSortedPairs <|
        List.map2 (,) (List.map fst wordCosts) wordCosts
  in
    case
      (maybePronouncer, maybeDeletionCosts, maybeSubCosts, maybeWordCosts)
    of
      (Just pronouncer, Just deletionCosts, Just subCosts, Just wordCosts) ->
        let
          result =
            Respell.respell
              { pronouncer = pronouncer
              , deletionCosts = deletionCosts
              , subCosts = subCosts
              , wordCosts = wordCosts
              }
              Respell.emptyCache <|
              String.join " " <| List.indexedMap (always << toString) sentence
        in
          result.respelling
      _ -> Nothing

costlessExample :
  List (List String) -> List (String, Float) ->
    List (String, List (String, Float)) -> List (String, Float) ->
    Maybe String
costlessExample sentence wordCosts subCosts deletionCosts =
  Maybe.map fst <| respellExample sentence wordCosts subCosts deletionCosts
