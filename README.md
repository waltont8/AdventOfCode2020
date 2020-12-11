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
