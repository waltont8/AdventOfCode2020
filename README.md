# AdventOfCode2020
Advent of code 2020 - I'm using haskell stack to build this code and just editing the default someFunc in Lib.hs. If you want to rebuild any of these, copy them into a stack project and fix up build-depends in the cabal file.

## Day 1: Report Repair
I was expecting to have to optimize this but the list comprehensions I tried first finished instantly

## Day 2: Password Philosophy
Browsing around the internet, a lot of Haskell solutions seem to make use of the various parser combinators. Didn't really seem worth it for this one so I just pattern matched the file. If any other exercises build on this and increase the input complexity then I might have a rethink. Possibly interesting was my use of fromEnum to turn True into 1 so I could sum an array of bools and count the true values. I could have written a shorter solution but I like to spread things out a bit under the where clause, I think it makes the code look nicer.

## Day 3: Toboggan Trajectory
Most solutions seem to have used modulus for the infinite ski slope but I'd used cycle, this being haskell. Fairly straightforward. I built the part 1 solution to accept parameters for the step in anticipation of part 2 so there is only one solution here.

## Day 4: Passport Processing
I took a bit longer on part 1, putting the data into a list of tuples, as I figured I would need some sort of database to query in part 2. I found this one a bit annoying, it's right on the border of "can hack it together with list operations" and "needs a parser or the regexp library". If we have to come back to this I'll probably rewrite as a parser into record structures.

## Day 5: Binary Boarding
I see that the seat code is two binary numbers together and this can be used to take some shortbuts, but I just like recursive subdivision, it's pretty. Also, you never know what's coming in part 2 so it pays not to be too hacky. I didn't /really/ finish part 2, it's obvious from printing out the list of empty seats which one it is, so I just went with that.

## Day 6: Custom Customs
Really felt like there should be an easy arrow solution here, with the results from each group being piped through count and frequency analysis and then being filtered back together. A filter that takes answers one entry at a time. Couldn't quite see how to do that so I just stepped through the easy way.

## Day 7: Handy Haversacks
Tried to keep with the spirit of things and get the answers fast. I stripped out all punctuation, whitespaces and the letter s then split each line on "contain" and turned it into a tuple. This gave me an array of (colour, contents) which I search with isInfixOf. Very easy for part 1, unsurprisingly part 2 made a bit of a mess but it gets the answer...

## Day 8: Handheld Halting
Flashbacks to intcode from last year. Happy enough with part 1. Part 2 is very inefficient. You could probably build a graph from the program, identify the loops in part 1 and then break out directly but that would have taken too long to figure out. Just bruteforce the answer, it takes microseconds.

## Day 9:Encoding Error
Pretty easy again. List comprehension to generate the possible ranges and then a liberal sprinkling of arrows in part 2.

## Day 10:Adapter Array
Part one was just some data fiddling. Part two was much harder. I tried a brute force approach, because often this just works with haskell because of some behind-the-scenes magic. Not this time! Had to have a think about how the list could be split into subsections on gaps of 3 that couldn't be moved. There are two functions here. One that spits the list on 3gaps and one that finds all possible routes through the sub lists.

## Day 11: Seating System
I always find dealing with multi-dimentional data to be a huge pain in Haskell and you don't get much for free out of the compiler either. Usually I would have had a Data.Map match the coordinates to the data and then hidden this all behind lookups. This time I kept a list of tuples of position and data. I need to spend some time thinking about what the best way to do this is when I'm not under any time pressure. The difference between part 1 and 2 here is just a single function so you can swap the commented out code over to get part 1. Given how much of a mess I get in with haskell and mazes I spent some time with types.

## Day 12: Rain Risk
Easy again. I find it's easy to get confused when rotating things so I wrote out the turnLeft and turnRight function in a long form. Different files for part 1 and part 2, there didn't seem to be an obvious transition between the two...


## Day 13: Shuttle Search
I didn't like this one. Part 2 clearly had some mathsy answer I didn't know, google says it was chinese remainder theorem, haskell has a chineseremainder function. Stepping through all numbers, increasing the step size to a product of buses we've matched so far would have been my fallback. That would probably have been fast enough.

## Day 14: Docking Data
That felt more programmery. Used a Data.Map to represent memory. Haven't done much bit twiddling in haskell, I would normally do that in C or C++, so that was interesting. I am sure there is a nicer way of generating all the valid permutations of each address, but a quick hoogle failed to find anything so I hacked something up with borrowing the bits out of a sequence of numbers. Seperate part 2 again.

## Day 15: Rambunctious Recitation
No input file. Pretty straightforward "Can you read the question" question. I started with a Map because Maps are performance magic and so part2 came for free.

## Day 16: Ticket Translation
Long one today. I only took part 2 as far as finding the column numbers and did the final calculation on a calculator, the final mapping code wasn't the interesting bit and would have been messy. Know when not to code. Still parsing things with split. I feel there must be a proof that any parser can be built with splits and joins...

## Day 17: Day 17: Conway Cubes
Keeping a list of active cells in a Data.Set, then turn the set into a list, generate neighbours of everything in the list and try to activate each cell in the list. Part2 was going to be run for 1 billion turns or run in n dimensions so I invested more time in part 1 which meant part 2 came for free. This was a good one, I like puzzles in higher dimensions.

## Day 18: Operation Order
Just used Parsec and a calculator grammar I had hanging around. Easiest one yet as haskell did all the heavy lifting.

## Day 19: Monster Messages
Tricky one today. Generate a parser from a grammar in part 1. Same thing in part 2 but a more complicated grammar with recursion and where greedy parsing fails. The parsing code is a bit c&p but it's supposed to be a race. I think this one took me the longest so far.

## Day 20: Jurassic Jigsaw
Part 1 was quite quick, just hack around and find the tiles with only two matching edges. Moment of panic when I thought there matches might not be unique, but each edge matches only one other. Part 2 was again easy but involved sooo many steps, took a few hours. I just stream of consciousness the code for these easy ones, I'm sure you could refactor it to nothing. There are a few hard coded bits in there just to speed it along.

## Day 21: Allergen Assessment
Took a while to get my head around what was being asked for in this one, needed to work through the examples. The "Allergens aren't always marked" hint makes it all quite hard to think about. I tend to over engineer part 1s and I did so here, so part 2 was easy.

## Day 22: Crab Combat
Today's puzzle was about "can you read the question carefully". Other than that, the format really fit well with haskell. Almost looks like the question was designed with haskell in mind.

## Day 23: Crab Cups
Part 1, very simple, instant answer. Part 2, the haskell magic failed me and the result was looking like 19 days to completion. Impressively, it was maxing out 7 of the 8 cores on this laptop so autoparallelization was happening. I tried a faster data structures (sequence ) with the same algorithm but did no achieve the needed speedup. I wrote a new algorithm using maps to hold a linked list, and improved the min, max and elem algorithms. This got the job done in about 25s.
