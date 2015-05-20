{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Web.YahooPortfolioManager.Foundation where

import Yesod

data YahooPortfolioManagerSite = YahooPortfolioManagerSite

mkYesodSubData 
    "YahooPortfolioManagerSite" 
    $(parseRoutesFile "Web/YahooPortfolioManager/config/routes")
