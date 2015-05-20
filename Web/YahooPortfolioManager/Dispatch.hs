{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Web.YahooPortfolioManager.Dispatch where

import Text.Hamlet (hamletFile)
import Web.YahooPortfolioManager.Foundation 
import Yesod

import Data.Default
import Yesod.Default.Util

-- And we'll spell out the handler type signature.
getYpmHomeR :: Yesod master => HandlerT YahooPortfolioManagerSite (HandlerT master IO) Html
getYpmHomeR = lift $ defaultLayout $ do
    setTitle "Portfolio Monitor - Home"
    toWidget $(hamletFile "Web/YahooPortfolioManager/templates/ypmHome.hamlet")

instance Yesod master => YesodSubDispatch YahooPortfolioManagerSite (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesYahooPortfolioManagerSite)
