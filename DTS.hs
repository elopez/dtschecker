{-# LANGUAGE RecordWildCards #-}

import Control.Applicative hiding ((<|>))

import Text.Parsec.Language
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec

data PropertyValue = String String
		   | Empty
		   deriving (Show)

data DTS = Block String [DTS]
	 | Property String PropertyValue
	 | Label String DTS
	 deriving (Show)

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
		at_address = do { char '@'; a <- unit_address; return $ '@' : a }
		full_name = do { n <- name; a <- option "" at_address; return $ n ++ a }

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

directives :: Parser String
directives = lexeme $ between (string "/") (string "/") (choice [string "include", string "dts-v1"]) <* manyTill anyChar (string "\n")

dts :: Parser DTS
dts = whiteSpace *> directives *> block
