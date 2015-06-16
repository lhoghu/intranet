{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import Settings
import Text.Hamlet (hamletFile)
import Web.Notebook.App
import Web.YahooPortfolioManager.App
import Yesod

data Intranet = Intranet { 
    getYahooPortfolioManagerSite :: YahooPortfolioManagerSite,
    getNotebook :: Notebook    
}

mkYesodData "Intranet" $(parseRoutesFile "config/routes")

instance Yesod Intranet where
    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
