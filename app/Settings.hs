{-# LANGUAGE TemplateHaskell       #-}
module Settings where

import Data.Default
import Language.Haskell.TH.Syntax (Q, Exp)
import Yesod.Default.Util

widgetFile :: String -> Q Exp
widgetFile = widgetFileNoReload def
