{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Handlers where

import Foundation
import Settings
import Web.YahooPortfolioManager.App
import Yesod

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Intranet - Home" 
    $(widgetFile "home")

getIndexR :: Handler Html
getIndexR = defaultLayout $ do
    setTitle "Intranet - Index" 
    $(widgetFile "index")
