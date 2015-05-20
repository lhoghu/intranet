{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Web.YahooPortfolioManager.Handlers where

import Text.Hamlet (hamletFile)
import Web.YahooPortfolioManager.Foundation 
import Yesod

type YPMS = YahooPortfolioManagerSite

ypmLayout :: Yesod master => Html
                          -> HtmlUrl (Route master)
                          -> HandlerT YPMS (HandlerT master IO) Html
ypmLayout title widget = do
    -- Need to lift the child site url to the parent site in order
    -- to create links within the subsite: see use of toParent in 
    -- sidebar.hamlet (http://www.yesodweb.com/blog/2013/03/big-subsite-rewrite)
    toParent <- getRouteToParent
    let content = widget 
    lift $ defaultLayout $ do
        setTitle title
        toWidget $(hamletFile "Web/YahooPortfolioManager/templates/sidebar.hamlet")

getYpmHomeR :: Yesod master => HandlerT YPMS (HandlerT master IO) Html
getYpmHomeR = 
    ypmLayout 
        "Portfolio Monitor - Home"
        $(hamletFile "Web/YahooPortfolioManager/templates/ypmHome.hamlet")
