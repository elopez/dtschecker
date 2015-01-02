module DTS.Pretty where

import DTS
import Text.PrettyPrint

ppDTS :: [DTS] -> Doc
ppDTS = ppDTS'' (-1)
	where
		ppDTS' l (Block n c)    = text n <> text "{" $+$
					  ppDTS'' (l+1) c $+$
					  text "};"
		ppDTS' _ (Property k v) = sep [text k, text "=", text $ show v, text ";"]
		ppDTS' l (Label n e)    = text n <> text ": " <> ppDTS' l e
		ppDTS' _ (Include f)    = sep [text "/include/", text f]
		ppDTS' _ (Version n)    = text ("/dts-v" ++ show n ++ "/;")
		ppDTS'' l               = foldl (\d x-> d $+$ nest (l+1) (ppDTS' (l+1) x)) empty
