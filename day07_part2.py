import json
from itertools import chain

### recursively count bags
def countInner(d, l, c):
    if len(l):
        return (countInner( d,
                    list(chain.from_iterable([d[x] for x in l])), # Flattens a list of lists into a list
                    c + len(l)))
    else:
       return c

def removeSuffix(input_string, suffix):
    if suffix and input_string.endswith(suffix):
        return input_string[:-len(suffix)]
    return input_string

#### Strip bag or bags from the end of a string
def stripBags(s):
    s = removeSuffix(s, " bag")
    s = removeSuffix(s, " bags")
    return (s)

### For a list of strings like ["n colour", "m colour"]
### pull out the numbers and then
### return a super list of "colour", "colour", "colour".....
def expandNumbers(ls):
    ret = []
    for s in ls:
        num = s[:1]
        colour = s[2:]
        for i in range(0, int(num)):
            ret.append(colour)

    return(ret)

#### Main
rows = []
with open("input.txt") as f:
    rows = f.readlines()

#Drop newlines and spaces
rows = [x.strip() for x in rows]

#Drop last fullstop
rows = [x[:-1] for x in rows]


bagDict = {}

for row in rows:
    (outer,inner) = row.split(" bags contain ")
    innerList = []
    if (inner != "no other bags"):
      innerList = inner.split(", ")
      innerList = [stripBags(x) for x in innerList]
      innerList = expandNumbers(innerList)
      bagDict[outer] = innerList
    else:
      bagDict[outer] = []

# Note the -1 adjustment to remove "shiny gold" because that doesn't count
total = countInner(bagDict, ["shiny gold"], 0) - 1

print("Total is " + str(total))


