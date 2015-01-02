module Checker where

import DTS

import Data.List

data Check = OK | Err String | Skip

data Rule = Required [String]
	  | Optional [String]
	  | Obsolete [String]
	  | Checks   [(DTS -> Check)]
	  | Warn     String
	  | Error    String

data Ruleset = Ruleset (String -> Bool) [Rule]

-- Generator to match a specific compatible
compatibleEquals :: String -> (String -> Bool)
compatibleEquals = (==)

-- Generator to match a compatible by prefix
compatibleStartsWith :: String -> (String -> Bool)
compatibleStartsWith = isPrefixOf

-- Generator to match a set of compatibles
compatibleIsOneOf :: [String] -> (String -> Bool)
compatibleIsOneOf l = (\x -> elem x l)

-- Example ruleset
rules = Ruleset
	(compatibleIsOneOf ["allwinner,sun4i-a10-wdt", "allwinner,sun6i-a31-wdt"])
	[Required ["reg"],
	 Optional ["clocks"],
	 Checks   [undefined]
	]
