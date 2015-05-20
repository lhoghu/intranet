{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Dispatch where

import Foundation
import Handlers
import Yesod

mkYesodDispatch "Intranet" resourcesIntranet

