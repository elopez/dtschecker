import Control.Applicative
import System.Environment

import Checker

main :: IO ()
main = do params <- getArgs
	  runTests (params !! 0)
