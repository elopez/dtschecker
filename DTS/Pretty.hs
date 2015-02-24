module DTS.Pretty where

import DTS
import Text.PrettyPrint

commaList [x]    = x
commaList (x:xs) = x <> comma <+> commaList xs

ppDTSValue :: PropertyValue -> Doc
ppDTSValue (String s) = text $ show s
ppDTSValue (Number n) = text $ show n
ppDTSValue (Handle h) = text $ "&" ++ h
ppDTSValue (List l)   = text "<" <> sep (map ppDTSValue l) <> text ">"
ppDTSValue (Macro m)  = text m
ppDTSValue (Other o)  = text o <> text " /* unparseable value */"
ppDTSValue (Empty)    = text "/* empty */"

ppDTS :: [DTS] -> Doc
ppDTS = ppDTS'' (-1)
	where
		ppDTS' l (Block n c)          = text n <> text " {" $+$
						ppDTS'' l c $+$
						text "};"
		ppDTS' _ (Property k [Empty]) = text k <> text ";"
		ppDTS' _ (Property k v)       = text k <> text " = " <> commaList (map ppDTSValue v) <> text ";"
		ppDTS' l (Label n e)          = text n <> text ": " <> ppDTS' l e
		ppDTS' _ (Include f)          = sep [text "/include/", text f]
		ppDTS' _ (Version n)          = text ("/dts-v" ++ show n ++ "/;")
		ppDTS'' l                     = foldl (\d x-> d $+$ nest (l+1) (ppDTS' (l+1) x)) empty
