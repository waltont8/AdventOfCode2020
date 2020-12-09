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
