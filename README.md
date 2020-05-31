# phrase2unit &#x1F4D6;&#x1F52C;

[![XKCD 2260 - Reaction Maps](https://imgs.xkcd.com/comics/mbmbam.png)](//xkcd.com/2312)

<sup>&#xa9; Randall Monroe, 2020</sup>

This tool interprets phrases as if they were a sequence of units, slicing the phrase into unit symbols that cancel out into the smallest unit that covers the most of the phrase. It thereby implements [XKCD 2312](//xkcd.com/2312). The engine inserts [SI prefixes](//physics.nist.gov/cuu/Units/prefixes.html) where they make the resulting unit smaller. It uses unit symbols, common names, and conversions to SI [provided by Wikipedia](//en.wikipedia.org/wiki/Module:Convert/documentation/conversion_data), specifically [this Lua table](//en.wikipedia.org/wiki/Module:Convert/data) that is generated from that page.

Live implementation at <//units.lam.io>

## Technical overview

The engine is written in Haskell and is made up of two parts:

1. A dynamic-programming algorithm that finds the unit that bridges the current string position to a later one with the smallest resulting SI unit, and

1. A heuristic [knapsack-problem](//en.wikipedia.org/wiki/Knapsack_problem) solver to convert the final SI unit back to a more familiar worded form (e.g. m/s &rarr; speed).

Since the DP and knapsack solvers aren't optimal, the results aren't always strictly minimal, but they're usually pretty good and small.

Further details on implementation can be found [on my blog](//lam.io/writing/p2u).