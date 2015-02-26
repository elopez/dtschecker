module Checker where

import Prelude hiding (catch)

import DTS
import DTS.Toolkit

import Control.Applicative
import Control.Monad
import Control.Exception
import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MultiMap

import Debug.Trace

data Check = OK | Err String | Skip deriving (Eq)

instance Show Check where
	show (OK) = ""
	show (Err s) = s
	show (Skip) = ""

data Rule = Required [String]
	  | Obsolete [String]
	  | Checks   [(DTSToolkit -> DTS -> Check)]
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

-- Function to match all nodes
genericRules :: String -> Bool
genericRules = const True

property :: String -> (DTSToolkit -> [PropertyValue] -> Check) -> (DTSToolkit -> DTS -> Check)
property n f dtk (Property k v) | k == n = f dtk v
property _ _ _ _ = Skip

propertyStartsWith :: String -> (DTSToolkit -> [PropertyValue] -> Check) -> (DTSToolkit -> DTS -> Check)
propertyStartsWith n f dtk (Property k v) | n `isPrefixOf` k = f dtk v
propertyStartsWith _ _ _ _ = Skip

propertyEndsWith :: String -> (DTSToolkit -> [PropertyValue] -> Check) -> (DTSToolkit -> DTS -> Check)
propertyEndsWith n f dtk (Property k v) | n `isSuffixOf` k = f dtk v
propertyEndsWith _ _ _ _ = Skip

-- Example ruleset
rules = Ruleset
	(compatibleIsOneOf ["allwinner,sun4i-a10-wdt", "allwinner,sun6i-a31-wdt"])
	[Required ["reg"],
	 Checks   [property "reg" (\_ v -> if (v !! 1) == Number 4 then OK else Err "nope")]
	]

cellSizeMatches sizeparam dtk cells = if length errors == 0 then OK else Err errors
	where
		checkCell (List (x:xs)) = case is_handle x of
					       Nothing  -> ["list starts without a valid handle"]
					       Just hdl -> case length_ok hdl xs of
								True  -> []
								False -> bigcell_ok x hdl xs
		is_handle (Handle x) = getNodeByHandle dtk x
		is_handle _ = Nothing
		parse_size (Just [List [Number n]]) = n
		length_ok hdl xs = parse_size (getProperty hdl sizeparam) == length xs
		bigcell_ok x hdl xs = let sz = parse_size (getProperty hdl sizeparam) in
					if length xs > sz then checkCell (List $ drop sz xs)
					else ["wrong number of specifiers for " ++ (show x)]
		errors = unlines $ concat $ map checkCell cells

rules2 = Ruleset
	 genericRules
	 [Checks [property "clocks" $ cellSizeMatches "#clock-cells",
		  propertyEndsWith "gpios" $ cellSizeMatches "#gpio-cells",
		  property "interrupts-extended" $ cellSizeMatches "#interrupt-cells"]]

isBlockCompatible :: (String -> Bool) -> DTS -> Bool
isBlockCompatible f b@(Block _ _) = or $ map f $ getNodeCompatibles b

checkNode :: DTSToolkit -> [Rule] -> DTS -> (String, [Check])
checkNode dtk r b@(Block n p) = (n, concat $ checkNode' <$> r)
	where
		getKeyVal (Property k v) = [(k, v)]
		getKeyVal _              = []
		keyval = concat $ map getKeyVal p
		keys = map fst keyval
		kvmap = Map.fromList $ keyval
		checkNode' (Required l) = case l \\ keys of
					       [] -> return OK
					       p  -> return $ Err $ "Missing properties: " ++ show p
		checkNode' (Obsolete l) = case keys `intersect` l of
					       [] -> return OK
					       p  -> return $ Err $ "Obsolete properties in use: " ++ show p
		checkNode' (Checks l)   = l <*> [dtk] <*> (b : p)

getErrors :: [(String, [Check])] -> [(String, String)]
getErrors l = concat $ map (\(n, v) -> case drop_ok v of
					    "" -> []
					    x  -> [(n, x)] ) l
	where
		drop_ok l = concat $ map (\(Err e) -> e) $ filter (\x -> x /= OK && x /= Skip) l

verifyRuleset :: DTSToolkit -> Ruleset -> [(String, [Check])]
verifyRuleset dtk (Ruleset f r) = map (checkNode dtk r) blocks
	where
		mm = nodesByCompatible dtk
		compatibles = filter (\l -> or $ map f l) $ MultiMap.keys mm
		blocks = concat $ map ((flip MultiMap.lookup) mm) compatibles

runTests file =  do { k@(Kit x y z) <- fileToKit file; print $ getErrors $ verifyRuleset k rules2 }
