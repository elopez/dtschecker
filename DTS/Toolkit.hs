module DTS.Toolkit (DTSToolkit(..), fileToKit, getProperty, getNodeCompatibles) where

import DTS

import Control.Applicative hiding ((<|>), many)
import Data.List
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MultiMap
import System.FilePath.Posix
import Text.ParserCombinators.Parsec

data DTSToolkit = Kit { tree              :: [DTS]
		      , getNodeByHandle   :: String -> Maybe DTS
		      , nodesByCompatible :: MultiMap [String] DTS
		      }

-- similar to nub, but leaves the last element instead of the first
unique :: (Eq a) => [a] -> [a]
unique = reverse . nub . reverse

parseFile :: FilePath -> IO [DTS]
parseFile f = do content <- readFile f
		 case parse dts f content of
			Right d -> checkIncl d
			Left _  -> return []
	where
		parseOther p = parseFile (replaceFileName f p)
		checkIncl :: [DTS] -> IO [DTS]
		checkIncl []               = return []
		checkIncl ((Include p):xs) = do { x <- parseOther p ; checkIncl (x ++ xs) }
		checkIncl (x:xs)           = (:) <$> pure x <*> checkIncl xs

mergeDTS dt = (unique not_blocks) ++ mergeDTS' (sort blocks)
	where
		is_block (Block _ _) = True
		is_block _           = False
		(blocks, not_blocks) = partition is_block dt
		mergeDTS' (xb@(Block x xc):yb@(Block y yc):xs) | x == y    = mergeDTS' ((Block x (mergeDTS (xc ++ yc))) : xs)
							       | otherwise = xb : (mergeDTS' (yb:xs))
		mergeDTS' x = x

findLabels x = findLabelsMap [] x
	where
		findLabelsMap p c                          = concat $ map (findLabels' p) c
		findLabels' path (Block ('&' : label) c)   = []
		findLabels' path (Block n c)               = findLabelsMap (path ++ [n]) c
		findLabels' path (Label label (Block n c)) = (label, path ++ [n]) : findLabelsMap (path ++ [n]) c
		findLabels' path _                         = []

replaceAlias x = cleanTreeMap x ++ findAliasMap x
	where
		labels = findLabels x
		findAliasMap c                    = concat $ map findAlias c
		findAlias (Block ('&' : label) c) = maybe [] (\p -> generateFillerBlocks p c) (lookup label labels) ++ findAliasMap c
		findAlias (Block n c)             = findAliasMap c
		findAlias _                       = []
		cleanTreeMap c                    = concat $ map cleanTree c
		cleanTree (Block ('&' : label) _) = []
		cleanTree (Label _ c)             = cleanTree c
		cleanTree (Block n c)             = [Block n (cleanTreeMap c)]
		cleanTree (Define _)              = []
		cleanTree x                       = [x]
		generateFillerBlocks (x:xs) e     = [Block x (generateFillerBlocks xs e)]
		generateFillerBlocks [] e         = e

loadDTS :: FilePath -> IO ([DTS], [(String, [String])])
loadDTS f = (,) <$> clean_dts <*> labels
	where
		raw_dts   = parseFile f
		clean_dts = (mergeDTS . replaceAlias) <$> raw_dts
		labels    = findLabels <$> raw_dts

getNode :: [DTS] -> [String] -> Maybe DTS
getNode ((Block t v):ts) [x]    | t == x    = Just (Block t v)
                                | otherwise = getNode ts [x]
getNode ((Block t v):ts) (x:xs) | t == x    = getNode v xs
                                | otherwise = getNode ts (x:xs)
getNode (_:ts) x                = getNode ts x
getNode [] _                    = Nothing

getProperty :: DTS -> String -> Maybe [PropertyValue]
getProperty (Block _ props) name = if length value == 0 then Nothing else Just v
	where
		is_property_named name (Property k _) = k == name
		is_property_named _ _ = False
		value = filter (is_property_named name) props
		(Property _ v) = head value

getNodeCompatibles :: DTS -> [String]
getNodeCompatibles node = value_to_list compat_value
	where
		compat_value = maybe [] id (getProperty node "compatible")
		value_to_list val = map (\(String s) -> s) val

compatibleMultiMap :: [DTS] -> MultiMap [String] DTS
compatibleMultiMap tree = foldl traverse_and_add MultiMap.empty tree
	where
		traverse_and_add t b@(Block _ v) = MultiMap.insert (getNodeCompatibles b) b $ foldl traverse_and_add t v
		traverse_and_add t _ = t

fileToKit :: FilePath -> IO DTSToolkit
fileToKit path = do (nodes, labels) <- loadDTS path
		    return $ Kit nodes (get_node_by_name nodes labels) (compatibleMultiMap nodes)
	where
		get_node_by_name nodes labels n = lookup n labels >>= (getNode nodes)
