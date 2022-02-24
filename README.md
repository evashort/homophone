# Homophone Generator
Finds a different set of words that sound like the input

Single-page client-side web app written in Elm

Try it out at https://evashort.com/homophone/

## Building from source

1. Install [Elm Platform 0.18](https://guide.elm-lang.org/install.html)

1. Run `git clone https://github.com/evashort/elm-pairing-heap.git` so that
`elm-pairing-heap` and `homophone` are side-by-side in the same directory

1. Run `elm-make src/Main.elm --output elm.js --warn`

1. Open `index.html` in Firefox (Chrome prevents it from loading the data
files)

## Running tests

1. Install [Node.js v6.x.x LTS](https://nodejs.org/en/)

1. Run `npm install -g elm-test`

1. From the `homophone` directory, run `elm-test`

## Generating data files

1. Install [Python 3](https://www.python.org/downloads/)

1. Run `python getData.py` to populate the `cache/` directory. The total size
will be 1.17 GB. If it gets interrupted, delete whichever file it was in the
middle of generating and run it again.

1. After editing files in the `handcraft/` directory, delete certain files in
the `data/` and `cache/` directories and run `python getData.py` again. Refer
to the individual `handcraft/` files for more information.

## How it works

To be honest I'm hoping I'll be able to explain the algorithm without
referring to the code, I barely understand the code anymore and I would write
completely different code if I were implementing it today. Some of my most
important insights have to do with pre-processing the pronunciation dictionary,
and they are captured in [handcraft/shortenings.txt](handcraft/shortenings.txt)
- Treat all unstressed vowels as the same sound (represented by ' )
- Some vowel sounds have semivowel (glide) counterparts, specifically EE -> Y,
OO -> W and ER -> R. When one of these vowels appears in the pronunciation
dictionary, add its semivowel counterpart afterward, for example EE becomes EE
Y. Then allow two consecutive semivowels to collapse into one when comparing
pronunciations. This way "see your" and "see or" are equivalent.

The biggest mistake I made during this project was allowing words to be
pronounced multiple different ways. Don't do this. There are very few words
with more than one pronunciation in the dictionary, and it makes the code
vastly more complex. Just choose an arbitrary pronunciation for each word and
tweak those choices manually if they produce bad results.

The reason my code doesn't produce a list of multiple pronunciations is that
there are an exponential number of them, and not generating all of them is an
important optimization. For example, consider the sentence "see your packet".
Here are some possible results:
1. see your packet
1. see or packet
1. see your pack it
1. see or pack it

There are two independent choices and therefore 4 possible results. 3
independent choices would mean 8 possible results and so on. Therefore we
eliminate all but the highest-scoring possibility as early as possible.

There are two factors that determine the score:

1. Substitution/deletion/insertion costs. These are captured in
[handcraft/groups.txt](handcraft/groups.txt)

    In the code, insertions are called "rabbits" which is a reference to
    pulling a rabbit out of a hat; it appears out of thin air. For example
    "a pit" can become "up hit" with a slight penalty for inserting an H.

    As an example of substitution, "a pit" can also become "up pit" with a
    slight penalty for replacing "p" with "pp". This is not considered an
    insertion because "p" is not allowed to appear out of thin air.

1. Boundary costs. These are tricky to get right, but the existing code is
unnecessarily complicated and far from ideal. Here's the basic concept:
    - "a pit" -> "up hit" is best (lowest cost).
    - "a pit" -> "up pit" is second best.
    - "a pit" -> "a pit" is worst.
    - "your hat" -> "your at" is also worst. This is the sort of edge case
    that makes things confusing.

    The reason boundary costs are so complicated in the existing code is
    because I wanted to allow many-to-many phoneme substitutions. This is
    not necessary. The only substitutions I ended up using were 1:1 or 1:2.

1. How common the words are. I used Google Books ngram data to add a penalty
for using rare/obscure words.

Going back to the "see your packet" example, notice that the space between
"your" and "packet" serves as a sort of checkpoint. The partial results "see
your" and "see or" put us in the same situation, allowing the same
possibilities for continuation. Therefore we want to generate both partial
results and eliminate one of them before extending either of them. Partial
results are stored in a priority queue, and in each iteration the one that
"covers" the fewest input phonemes is popped from the queue for continuation.

Note that "see your" and "see or" will have the same priority in the queue.
You should remove items from the queue in batches of equal priority and only
keep the one with lowest cost.

Just to clarify, I'm using the terms "priority", "checkpoint", and "number of
input phonemes covered" to refer to the exact same concept from slightly
different perspectives.

Only sequences of complete words are added to the priority queue, so you'll
never have to compare "see your" and  "see yo r-" because "r" is not a complete
word. To put it another way, some partial results may leapfrog a checkpoint and
that's okay.

"Number of input phonemes covered" is not actually a straightforward concept.
For example, "taxi" can be replaced with "tax see". Even though "tax" covers
all but the last input phoneme, the second-to-last phoneme reappears in the
following word. This is the part of the algorithm that's really hard to get
right. When in doubt, remember that items in the queue should have the same
priority if and only if the possibilities for continuation are identical.

At this point I've explained most of the algorithm. The remaining problem is to
figure out which words are valid continuations. For this you can do a recursive
depth-first search. First, ask which phonemes can come next based on the input
phonemes and the allowed substitutions/insertions/deletions. Then make a
recursive call for each of those phonemes. The recursion stops when the phoneme
sequence is not found at the start of any pronunciation in the dictionary. To
make this efficient, you'll want to sort the dictionary by pronunciation. For
example if you make a recursive call with the phoneme sequence "me", it only needs
to consider words in the range "Mebane" to "Mezvinsky". The recursion stops when
the range is empty.
