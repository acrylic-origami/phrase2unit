# phrase2unit &#x1F4D6;&#x1F52C;

[![XKCD 2260 - Reaction Maps](https://imgs.xkcd.com/comics/mbmbam.png)](//xkcd.com/2312)

<sup>&#xa9; Randall Monroe, 2020</sup>

This tool interprets phrases as if they were a sequence of units, slicing the phrase into unit symbols that cancel out into the smallest unit that covers the most of the phrase. It thereby implements [XKCD 2312](//xkcd.com/2312). The engine inserts [SI prefixes](//physics.nist.gov/cuu/Units/prefixes.html) where they make the resulting unit smaller. It uses unit symbols, common names, and conversions to SI [provided by Wikipedia](//en.wikipedia.org/wiki/Module:Convert/documentation/conversion_data), specifically [this Lua table](//en.wikipedia.org/wiki/Module:Convert/data) that is generated from that page.

Live implementation at <https://units.lam.io>

## Technical overview

The engine is written in Haskell and is made up of two parts:

1. A dynamic-programming algorithm that finds the unit that bridges the current string position to a later one with the smallest resulting SI unit, and
1. A heuristic [knapsack-problem](//en.wikipedia.org/wiki/Knapsack_problem) solver to convert the final SI unit back to a more familiar worded form (e.g. m/s &rarr; speed).

Since the DP and knapsack solvers aren't optimal, the results aren't always strictly minimal, but they're usually pretty good and small.

Further details on implementation can be found [on my blog](//lam.io/projects/p2u).

## Installation

This is a Python-Haskell-React project. Yes, it's time to get funky with package managers.

1. Install and build using:

  ```
  $ npm i
  pip install -r < requirements.txt
  cabal build
  ```

2. This repo contains the data I used at the time of creation, so it's possible to run the server directly with `cabal run`, where it starts listening on port 8000 by [HappStack's] default.
3. However, if you want to update the data from Wikipedia and/or tweak the conversion:
	1. `prep/dat.lua` is the extracted set of Lua unit tables at [Wikipedia's Module:Convert/data](https://en.wikipedia.org/wiki/Module:Convert/data), with `all_units` exposed. `prep/convert.lua` just uses a tweaked version of [RXI's json.lua](https://github.com/rxi/json.lua) to ignore the weird mixed tables and yank the data into JSON like `cd prep; lua convert.lua > wikiunits.json`
	2. This data contains aliases of unit symbols that need to be flattened (e.g. `U.S.gal` -> `USgal`) and ratios whose constituent units need to be resolved (e.g. `L/100 km`). Further, some symbols that are referenced but not specified because they are prefixed. These include:

		```
		cm2, km2, um, mm, km, cm3, km3, ml, dL, ug, mg, kg, Mg, kPa, kN, kJ, MJ
		```
	
		I added these by hand to `wikiunits.json`.
		
		`prep/convert.py` converts the data to one the Haskell engine can use via `cd prep; python convert.py wikiunits.json utypes.json > ../hs-data/u2si.json`.
	3. There is a minified list of `utype`s called `hs-data/lim_utypes` for use by the knapsack solver that removes redundant options (like `volume per area` vs. `length`) and unitless utypes (like `gradient`) to help it solve faster.

The Wikipedia data is also not the most complete. Notably it's missing candela, and to my eye it's also sparse on electrical units (like Volt, Farad, Henry, etc.). I'm looking for better datasets (see #1) that should improve the quality of the solver's results.
