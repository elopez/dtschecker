{-# LANGUAGE RecordWildCards #-}

module DTS where

import Control.Applicative hiding ((<|>), many)

import Text.Parsec.Language
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec

import Data.List

import System.FilePath.Posix

data PropertyValue = String String
		   | Empty
		   deriving (Show, Eq)

data DTS = Block    { _name    :: String, _content :: [DTS] }
	 | Property { _key     :: String, _value   :: PropertyValue }
	 | Label    { _name    :: String, _element :: DTS }
	 | Include  { _file    :: String }
	 | Version  { _version :: Int }
	 deriving (Show)

instance Eq DTS where
	(Property s1 _) == (Property s2 _) = s1 == s2
	(Block s1 _)    == (Block s2 _)    = s1 == s2
	(Label s1 _)    == (Label s2 _)    = s1 == s2
	(Version i1)    == (Version i2)    = i1 == i2

instance Ord DTS where
	(Block s1 _) `compare` (Block s2 _) = s1 `compare` s2

-- similar to nub, but leaves the last element instead of the first
unique :: (Eq a) => [a] -> [a]
unique = reverse . nub . reverse

-- Basic token parser for DTS
Tok.TokenParser { .. } = dts_tokens
	where
		dts_tokens = Tok.makeTokenParser dts_lang
		dts_lang = emptyDef { Tok.commentStart = "/*"
				    , Tok.commentEnd = "*/"
--				    , Tok.identStart = nodeNameChar
--				    , Tok.identLetter = nodeNameChar
				    , Tok.opStart = oneOf "<>={};,\""
				    , Tok.opLetter = oneOf "<>={};,\""
				    , Tok.reservedOpNames = ["=", "<", ">",
							     "{", "}", ";",
							     ",", "\""]
				    }

nodeLabel :: Parser DTS -> Parser DTS
nodeLabel p = Label <$> label_name <*> p <?> "node label"
	where
		label_name = lexeme $ manyTill (alphaNum <|> oneOf "_-") colon

-- ePAPR 2.2.1.1 Node Name Requirements
nodeName :: Parser String
nodeName = lexeme $ full_name <|> root
	where
		root = string "/" <?> "root node name"
		name = many1 (alphaNum <|> oneOf ",._+-") <?> "node name"
		unit_address = name <?> "unit address"
		at_address = (++) <$> string "@" <*> unit_address
		full_name = (++) <$> name <*> option "" at_address

-- ePAPR 2.2.4.1 Property Names
propertyName = lexeme name
	where
		name = many1 (digit <|> lower <|> oneOf ",._+-?#") <?> "property name"

-- placeholder for any value
propertyValue :: Parser PropertyValue
propertyValue = String <$> lexeme (manyTill anyChar (lookAhead semi))

-- ePAPR Appendix A, DTS Format v1
property :: Parser DTS
property = Property <$> propertyName <*> value
	where
		value = (reservedOp "=" *> propertyValue) <|> (lookAhead semi *> return Empty)

-- ePAPR Appendix A, DTS Format v1
block :: Parser DTS
block = try (nodeLabel block') <|> block'
	where
		block' = Block <$> nodeName <*> block_content
		block_content = braces stmts
		stmts = endBy (try property <|> block) semi

directive :: Parser DTS
directive = try (directive' "include" *> (Include <$> stringLiteral)) <|>
	    try (directive' "dts-v1" *> semi *> (return $ Version 1))
	    <?> "directive"
	where
		slash = string "/"
		directive' d = lexeme $ between slash slash $ string d

dts :: Parser [DTS]
dts = (++) <$> (whiteSpace *> many directive) <*> endBy block semi

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

replaceAlias x = dropLabelsMap x ++ findAliasMap x
	where
		labels = findLabels x
		findAliasMap c                     = concat $ map findAlias c
		findAlias (Block ('&' : label) c)  = maybe [] (\p -> generateFillerBlocks p c) (lookup label labels) ++ findAliasMap c
		findAlias (Block n c)              = findAliasMap c
		findAlias _                        = []
		dropLabelsMap c                    = concat $ map dropLabels c
		dropLabels (Block ('&' : label) _) = []
		dropLabels (Label _ c)             = dropLabels c
		dropLabels (Block n c)             = [Block n (dropLabelsMap c)]
		dropLabels x                       = [x]
		generateFillerBlocks (x:xs) e      = [Block x (generateFillerBlocks xs e)]
		generateFillerBlocks [] e          = e

loadDTS :: FilePath -> IO [DTS]
loadDTS f = (mergeDTS . replaceAlias) <$> parseFile f

