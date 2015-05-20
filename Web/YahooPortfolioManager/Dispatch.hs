{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Web.YahooPortfolioManager.Dispatch where

import Text.Hamlet (hamletFile)
import Web.YahooPortfolioManager.Foundation 
import Web.YahooPortfolioManager.Handlers
import Yesod

instance Yesod master => YesodSubDispatch YahooPortfolioManagerSite (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesYahooPortfolioManagerSite)
