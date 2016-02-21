module Tests where

import ElmTest exposing (..)
import List
import String

import CompletionDict
import Repronounce exposing (shallowCost, deepCost)
import Respell

all : Test
all =
  suite "Respell Suite"
    [ test
        "It can repeat the same rabbit in order to create the right leftovers" <|
        assertEqual
          (Just ("abca bcab cabcar", 3 * deepCost)) <|
          respellExample
            [["ar"]]
            [("abca", 0.0), ("bcab", 0.0), ("cabcar", 0.0)]
            [("", [("abc", 0.0)])]
            []
    , test
    "It stops repeating the rabbit after trying all possible leftovers" <|
      assertEqual
        Nothing <|
        respellExample
          [["ar"]]
          [("abca", 0.0), ("bcab", 0.0), ("cabca", 0.0)]
          [("", [("abc", 0.0)])]
          []
    , test
        "It can't avoid the WBP by deleting the rest of the word" <|
        assertEqual
          (Just ("at", 1.9 * deepCost)) <|
          respellExample
            [["ah"], ["up"]]
            [("a", 0.0), ("ah", 0.0), ("at", 0.0), ("p", 0.0), ("up", 0.0)]
            [("p", [("t", deepCost * 0.9)])]
            [("h", 0.0), ("u", 0.0)]
    , test
        "It can create multiple words within the same substitution or rabbit" <|
        assertEqual
          (Just ("a b c de f g", 3 * shallowCost + 3 * deepCost)) <|
          respellExample
            [["x"]]
            [("a", 0.0), ("b", 0.0), ("c", 0.0), ("de", 0.0), ("f", 0.0), ("g", 0.0)]
            [("", [("efg", 0.0)]), ("x", [("abcd", 0.0)])]
            []
    , test
        "When it deletes entire words, the WBP applies anywhere on the boundary" <|
        assertEqual
          (Just ("ah h ha", 3 * deepCost)) <|
          respellExample
            [["a"], ["uu"], ["u"], ["uu"], ["a"]]
            [("ah", 0.0), ("h", 0.0), ("ha", 0.0)]
            [("", [("hhh", 0.0)])]
            [("u", 0.0)]
    , test
        "When a word is fully replaced, boundaries within it get the semi-WBP" <|
        assertEqual
          (Just ("ado go", shallowCost + deepCost)) <|
          respellExample
            [["a"], ["cat"], ["o"]]
            [("ado", 0.0), ("go", 0.0)]
            [("cat", [("dog", 0.0)])]
            []
    , test
        "Boundaries at the edges of the original words still get the semi-WBP" <|
        assertEqual
          (Just ("bet dime", shallowCost + deepCost)) <|
          respellExample
            [["bed"], ["time"]]
            [("bet", 0.0), ("dime", 0.0)]
            [("dt", [("td", 0.0)])]
            []
    , test
        "It can choose a pronunciation that is completed by the other one" <|
        assertEqual
          (Just ("aha", deepCost)) <|
          respellExample
            [["ab", "a"], ["a"]]
            [("abra", 0.0), ("aha", 0.0)]
            [("", [("h", 0.0), ("r", 1.0)])]
            []
    , test
        "It can choose a pronunciation that is a completion of the other one" <|
        assertEqual
          (Just ("abra", deepCost)) <|
          respellExample
            [["ab", "a"], ["a"]]
            [("abra", 0.0), ("aha", 0.0)]
            [("", [("h", 1.0), ("r", 0.0)])]
            []
    , test
    "Once it chooses a pronunciation, it can't use parts of the other one" <|
      assertEqual
        Nothing <|
        respellExample
          [["ab", "cd"]]
          [("ad", 0.0), ("bc", 0.0)]
          []
          []
    , test
        "It can delete even when a substitution makes the word uncontinuable" <|
        assertEqual
          (Just ("c d", shallowCost + deepCost)) <|
          respellExample
            [["abh"]]
            [("c", 0.0), ("d", 0.0)]
            [("ab", [("cd", 0.0)])]
            [("h", 0.0)]
    , test
        "Smaller subproblems can overtake larger cheaper ones via a reward" <|
        assertEqual
          (Just ("gif", deepCost - 1000.0)) <|
          respellExample
            [["abc"]]
            [("def", 0.0), ("gif", 0.0)]
            [("a", [("g", 1000.0)]), ("ab", [("de", 0.0)]), ("b", [("i", -2000.0)]), ("c", [("f", 0.0)])]
            []
    , test
        "It stops accumulating reward when there are no more matching words" <|
        assertEqual
          (Just ("hhot", deepCost - 1999.0)) <|
          respellExample
            [["at"]]
            [("hhot", 0.0)]
            [("", [("h", -1000.0)]), ("a", [("o", 1.0)])]
            []
    , test
        "It can choose the more expensive word to avoid a substitution" <|
        assertEqual
          (Just ("cot", 2.0 + deepCost)) <|
          respellExample
            [["cot"]]
            [("cat", 1.0), ("cot", 2.0)]
            [("o", [("a", 2.0)])]
            []
    , test
        "It can make an expensive substitution to avoid the more exensive word" <|
        assertEqual
          (Just ("cat", 2.0 + deepCost)) <|
          respellExample
            [["cot"]]
            [("cat", 1.0), ("cot", 3.0)]
            [("o", [("a", 1.0)])]
            []
    , test
        "It distinguishes puzzles by whether the last substitution is a rabbit" <|
        assertEqual
          (Just ("wx y z", 2.5 * deepCost + 0.5 * shallowCost)) <|
          respellExample
            [["a"]]
            [("ax", 0.0), ("wx", 0.0), ("y", 0.0), ("z", 0.0)]
            [("", [("xyz", 0.0)]), ("a", [("wxyz", 1.5 * deepCost - 1.5 * shallowCost)])]
            []
    , test
        "It distinguishes puzzles by whether the last value is in the gray area" <|
        assertEqual
          (Just ("ax y z", 1.5 * shallowCost + deepCost)) <|
          respellExample
            [["abcd"]]
            [("ax", 0.0), ("wx", 0.0), ("y", 0.0), ("z", 0.0)]
            [("abcd", [("wxyz", 0.0)]), ("bcd", [("xyz", 1.5 * shallowCost)])]
            []
    , test
        "It distinguishes puzzles by whether there is a space on the boundary" <|
        assertEqual
          (Just ("s a x yc", 2.5 * deepCost)) <|
          respellExample
            [["s"], ["abc"]]
            [("a", 0.0), ("s", 0.0), ("x", 0.0), ("yc", 0.0)]
            [("", [("xy", 0.0)])]
            [("ab", 0.0), ("b", 0.5 * deepCost)]
    , test
        "It doesn't prematurely expand puzzles created by deletions alone" <|
        assertEqual
          (Just ("a x yc", 3 * deepCost)) <|
          respellExample
            [["a"], ["bc"]]
            [("a", 0.0), ("x", 0.0), ("yc", 0.0)]
            [("", [("xy", 0.0)])]
            [("ab", 1000.0), ("b", 0.0)]
    , test
        "If there's a space anywhere on the boundary, the gray area starts there" <|
        assertEqual
          (Just ("la dox", shallowCost + deepCost)) <|
          respellExample
            [["le"], ["hot"], ["ax"]]
            [("dox", 0.0), ("la", 0.0)]
            [("ota", [("ado", 0.0)])]
            [("e", 0.0), ("h", 0.0)]
    , test
        "If there's a space anywhere on the boundary, the gray area ends there" <|
        assertEqual
          (Just ("one pi", shallowCost + deepCost)) <|
          respellExample
            [["ox"], ["ate"], ["hi"]]
            [("one", 0.0), ("pi", 0.0)]
            [("xat", [("nep", 0.0)])]
            [("e", 0.0), ("h", 0.0)]
    , test
        "It can choose words that increase the length of the leftovers" <|
        assertEqual
          (Just ("p ii zza", 3 * deepCost)) <|
          respellExample
            [["a"]]
            [("ii", 0.0), ("p", 0.0), ("zza", 0.0)]
            [("", [("izz", 0.0), ("pi", 0.0), ("pzz", 1000.0)])]
            []
    ]

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
        Respell.respell
          { pronouncer = pronouncer
          , deletionCosts = deletionCosts
          , subCosts = subCosts
          , wordCosts = wordCosts
          } <|
          String.join " " <| List.indexedMap (always << toString) sentence
      _ -> Nothing
