{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Web.YahooPortfolioManager.Handlers where

import Control.Applicative
import Control.Monad.Trans (liftIO) 
import Data.Text (Text, pack)
import qualified Data.YahooPortfolioManager.DbAdapter as YD
import qualified Data.YahooPortfolioManager.Types as YT
import Text.Hamlet (hamletFile)
import Text.Printf
import Web.YahooPortfolioManager.Forms
import Web.YahooPortfolioManager.Foundation 
import Yesod

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

data Percent = Percent (Maybe Double) deriving Show

class Format a where
    format :: a -> Text

instance Format [Char] where
    format = pack 

instance Format Double where
    format x = pack $ printf "%.2f" x

instance Format a => Format (Maybe a) where
    format (Just x) = format x
    format Nothing = "-"

instance Format Percent where
    format (Percent (Just x)) = format $ 100.0 * x
    format (Percent Nothing) = "-" 

getYpmHomeR :: Yesod master => HandlerT YPMS (HandlerT master IO) Html
getYpmHomeR = do
    toParent <- getRouteToParent
    ypmLayout 
        "Portfolio Monitor - Home"
        $(hamletFile "Web/YahooPortfolioManager/templates/ypmHome.hamlet")

getYpmCurrentHoldingsR :: Yesod master => HandlerT YPMS (HandlerT master IO) Html
getYpmCurrentHoldingsR = do
    position <- liftIO $ YD.withConnection YD.fetchPositions 
    toParent <- getRouteToParent
    ypmLayout "Portfolio Monitor - Holdings" $ do
        $(hamletFile "Web/YahooPortfolioManager/templates/holdings.hamlet")

getYpmDividendsR :: Yesod master => HandlerT YPMS (HandlerT master IO) Html
getYpmDividendsR = do
    dividend <- liftIO $ YD.withConnection YD.fetchDividends
    toParent <- getRouteToParent
    ypmLayout "Portfolio Monitor - Dividends" $ do
        $(hamletFile "Web/YahooPortfolioManager/templates/dividends.hamlet")

fetchValue :: YD.Connection -> IO [YT.Portfolio]
fetchValue conn = do
    symbols <- YD.fetchSymbols conn 
    YD.populateQuotesTable conn symbols
    YD.updateFx conn
    YD.fetchPortfolio conn

getYpmCurrentValueR :: Yesod master => HandlerT YPMS (HandlerT master IO) Html
getYpmCurrentValueR = do
    position <- liftIO $ YD.withConnection fetchValue
    ypmLayout "Portfolio Monitor - Current Value" $ do
        $(hamletFile "Web/YahooPortfolioManager/templates/value.hamlet")

getYpmAddTransactionR :: Yesod master => HandlerT YPMS (HandlerT master IO) Html
getYpmAddTransactionR = do
    toParent <- getRouteToParent
    ypmLayout "Portfolio Monitor - Add Transaction" $ do
        $(hamletFile "Web/YahooPortfolioManager/templates/addTransaction.hamlet")

getYpmInputTransactionR :: Yesod master => HandlerT YPMS (HandlerT master IO) Html
getYpmInputTransactionR = do
    position <- runInputGet $ YT.Position
                <$> ireq (textToStringField symbolField) "possymbol"
                <*> ireq currencyField "poscurrency"
                <*> ireq dateField "posdate"
                <*> ireq doubleField "posposition"
                <*> ireq doubleField "posstrike"
    liftIO . YD.withConnection $ (flip YD.insertPosition) position
    getYpmCurrentHoldingsR

getYpmAddDividendR :: Yesod master => HandlerT YPMS (HandlerT master IO) Html
getYpmAddDividendR = do
    toParent <- getRouteToParent
    ypmLayout "Portfolio Monitor - Add Dividend" $ do
        $(hamletFile "Web/YahooPortfolioManager/templates/addDividend.hamlet")


getYpmInputDividendR :: Yesod master => HandlerT YPMS (HandlerT master IO) Html
getYpmInputDividendR = do
    dividend <- runInputGet $ YT.Dividend
                <$> ireq (textToStringField symbolField) "divSymbol"
                <*> ireq doubleField "divDiv"
                <*> ireq dateField "divDate"
    liftIO . YD.withConnection $ (flip YD.insertDividend) dividend
    getYpmDividendsR
