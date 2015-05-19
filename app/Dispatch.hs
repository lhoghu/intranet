{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Dispatch where

import Foundation
import Settings
import Yesod

mkYesodDispatch "Intranet" resourcesIntranet

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Intranet - Home" 
    $(widgetFile "home")

getIndexR :: Handler Html
getIndexR = defaultLayout $ do
    setTitle "Intranet - Index" 
    $(widgetFile "index")
