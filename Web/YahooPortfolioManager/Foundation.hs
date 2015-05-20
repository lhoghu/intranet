{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.YahooPortfolioManager.Foundation where

import Yesod

data YahooPortfolioManagerSite = YahooPortfolioManagerSite

mkYesodSubData 
    "YahooPortfolioManagerSite" 
    $(parseRoutesFile "Web/YahooPortfolioManager/config/routes")
