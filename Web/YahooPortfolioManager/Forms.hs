{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Web.YahooPortfolioManager.Forms where

import Data.Maybe
import Data.Text (Text, pack, unpack)
import Text.Regex
import Web.YahooPortfolioManager.Foundation 
import qualified Data.YahooPortfolioManager.Yahoo as Y
import Yesod

instance RenderMessage YPMS FormMessage where
    renderMessage _ _ = defaultFormMessage

textToStringField :: Functor m => Field m Text -> Field m String
textToStringField = convertField unpack pack

stringField :: (Monad m, Functor m) => 
               RenderMessage (HandlerSite m) FormMessage => 
               Field m String
stringField = textToStringField textField

symbolField :: (Monad m, MonadIO m, Functor m) => 
               RenderMessage (HandlerSite m) FormMessage => 
               Field m Text
symbolField = textField --check validate textField
    -- where 
    -- validate :: Text -> Either Text Text
    -- validate s = do
    --     r <- liftIO $ Y.validateSymbol (unpack s)
    --     if r 
    --         then (Right s) 
    --         else (Left "Symbol not recognised by Yahoo")

currencyField :: (Monad m, Functor m) => 
                 RenderMessage (HandlerSite m) FormMessage => 
                 Field m String
currencyField = textToStringField $ regexField 
                (mkRegex "^[A-Za-z]{3}$")
                "Currency should be a three-letter string (e.g. EUR)"

dateField :: (Monad m, Functor m) => 
               RenderMessage (HandlerSite m) FormMessage => 
               Field m String
dateField = textToStringField $ regexField 
                (mkRegex "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
                "Date should be in form 'yyyy-mm-dd'"

regexField :: (Monad m, Functor m) => 
             RenderMessage (HandlerSite m) FormMessage => 
             Regex ->
             Text ->
             Field m Text
regexField regex errMsg = check validate textField
    where
    validate :: Text -> Either Text Text
    validate f
        | isJust $ matchRegex regex (unpack f) = Right f
        | otherwise = Left errMsg

-- addDividendAForm :: AForm Handler DividendForm
-- addDividendAForm = DividendForm
--     <$> areq symbolField "Symbol" Nothing
--     <*> areq dateField "Date" Nothing
--     <*> areq doubleField "Dividend (per share)" Nothing
-- 
-- addDividendForm :: Html -> MForm Handler (FormResult DividendForm, Widget)
-- addDividendForm = renderTable addDividendAForm
