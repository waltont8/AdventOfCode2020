# AdventOfCode2020
Advent of code 2020 - I'm using haskell stack to build this code and just editing the default someFunc in Lib.hs. If you want to rebuild any of these, copy them into a stack project and fix up build-depends in the cabal file.

## Day 1: Report Repair
I was expecting to have to optimize this but the list comprehensions I tried first finished instantly

## Day 2: Password Philosophy
Browsing around the internet, a lot of Haskell solutions seem to make use of the various parser combinators. Didn't really seem worth it for this one so I just pattern matched the file. If any other exercises build on this and increase the input complexity then I might have a rethink. Possibly interesting was my use of fromEnum to turn True into 1 so I could sum an array of bools and count the true values. I could have written a shorter solution but I like to spread things out a bit under the where clause, I think it makes the code look nicer.
