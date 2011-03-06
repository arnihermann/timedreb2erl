module TemplateHelper where

import Paths_timedreb2erl (getDataFileName)

import System.IO.Unsafe (unsafePerformIO)

import Text.StringTemplate
import Text.StringTemplate.GenericStandard ()

withTemplate :: String -> StringTemplate String
withTemplate f = newSTMP $ unsafePerformIO (readFile (unsafePerformIO (getDataFileName $ "templates/" ++ f))) :: StringTemplate String

