{-# LANGUAGE RecordWildCards #-}

module DTS (DTS(..), PropertyValue(..), dts) where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec.Language
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

data PropertyValue = String String
		   | Number Int
		   | Handle String
		   | List [PropertyValue]
		   | Macro String
		   | Other String
		   | Empty
		   deriving (Show, Eq)

data DTS = Block    { _name    :: String, _content :: [DTS] }
	 | Property { _key     :: String, _value   :: [PropertyValue] }
	 | Label    { _name    :: String, _element :: DTS }
	 | Include  { _file    :: String }
	 | Version  { _version :: Int }
	 | Define   { _data    :: String }
	 deriving (Show)

instance Eq DTS where
	(Property s1 _) == (Property s2 _) = s1 == s2
	(Block s1 _)    == (Block s2 _)    = s1 == s2
	(Label s1 _)    == (Label s2 _)    = s1 == s2
	(Version i1)    == (Version i2)    = i1 == i2
	(Define d1)     == (Define d2)     = d1 == d2
	a == b                             = False -- is this right?

instance Ord DTS where
	(Block s1 _) `compare` (Block s2 _) = s1 `compare` s2

-- Basic token parser for DTS
Tok.TokenParser { .. } = dts_tokens
	where
		dts_tokens = Tok.makeTokenParser dts_lang
		dts_lang = emptyDef { Tok.commentStart = "/*"
				    , Tok.commentEnd = "*/"
--				    , Tok.identStart = nodeNameChar
--				    , Tok.identLetter = nodeNameChar
				    , Tok.opStart = oneOf "<>={};,\""
--				    , Tok.opLetter = oneOf "<>={};,\""
				    , Tok.reservedOpNames = ["=", "<", ">",
							     "{", "}", ";",
							     ",", "\""]
				    }

nodeLabel :: Parser a -> Parser String
nodeLabel e = lexeme (manyTill (alphaNum <|> oneOf "_-") e) <?> "node label"

labeledNode :: Parser DTS -> Parser DTS
labeledNode p = Label <$> nodeLabel colon <*> p

-- ePAPR 2.2.1.1 Node Name Requirements
nodeName :: Parser String
nodeName = lexeme $ full_name <|> root
	where
		root = string "/" <?> "root node name"
		name = many1 (alphaNum <|> oneOf ",._+-&") <?> "node name"
		unit_address = name <?> "unit address"
		at_address = (++) <$> string "@" <*> unit_address
		full_name = (++) <$> name <*> option "" at_address

-- ePAPR 2.2.4.1 Property Names
-- some DTs use uppercase letters on the names
propertyName = lexeme name
	where
		name = many1 (digit <|> lower <|> upper <|> oneOf ",._+-?#") <?> "property name"

manyTill1 p end = (:) <$> p <*> (manyTill p end)

cppMacro :: Parser String
cppMacro = do c <- many $ alphaNum <|> char '_'
	      p <- option "" $ parens $ many $ noneOf ")"
	      if length c > 0 && length p > 0 then return $ c ++ "(" ++ p ++ ")"
	      else if length c > 0 then return c
	      else if length p > 0 then return $ "(" ++ p ++ ")"
	      else fail "expected CPP-style macro application or constant"

-- Property values
propertyValue :: Parser PropertyValue
propertyValue = List <$> lexeme (angles (many1 propertyValue)) <|>
		Number <$> lexeme nat <|>
		String <$> lexeme stringLiteral <|>
		Handle <$> lexeme (char '&' *> many1 (alphaNum <|> oneOf "_-")) <|>
		Macro <$> lexeme cppMacro

-- ePAPR Appendix A, DTS Format v1
property :: Parser DTS
property = Property <$> propertyName <*> value
	where
		value = (reservedOp "=" *> sepBy1 propertyValue comma) <|>
			(lookAhead semi *> return [Empty]) <|>
			(\x -> [Other x]) <$> try (lexeme (manyTill1 anyChar (lookAhead semi)))

-- ePAPR Appendix A, DTS Format v1
block :: Parser DTS
block = try (labeledNode block') <|> block'
	where
		block' = Block <$> nodeName <*> block_content
		block_content = braces stmts
		stmts = endBy (try block <|> property) semi

directive :: Parser DTS
directive = try (directive' "include" *> (Include <$> stringLiteral)) <|>
	    try (directive' "dts-v1" *> semi *> (return $ Version 1)) <|>
	    try (lexeme $ string "#include" *> whiteSpace *> (Include <$> stringLiteral)) <|>
	    try (lexeme $ string "#include" *> whiteSpace *> (Include <$> ("include/"++) <$> (angles $ many1 $ noneOf ">"))) <|>
	    try (lexeme $ string "#define" *> (Define <$> (many1 $ noneOf "\n")))
	    <?> "directive"
	where
		slash = string "/"
		directive' d = lexeme $ between slash slash $ string d

dts :: Parser [DTS]
dts = (++) <$> (whiteSpace *> many directive) <*> endBy block semi
