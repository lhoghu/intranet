{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE MultiParamTypeClasses         #-}
--{-# LANGUAGE GADTs                         #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
--{-# LANGUAGE QuasiQuotes                   #-}
module Foundation where

import Settings
import Text.Hamlet (hamletFile)
import Web.Notebook.App
import Web.YahooPortfolioManager.App
import Yesod
import Database.Persist.Sqlite (runSqlPool, SqlBackend)
import Network.HTTP.Conduit (Manager, newManager)
import Database.Persist.Sqlite
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration
    , createSqlitePool, runSqlPersistMPool
    )
import Yesod.Form.Nic (YesodNic)

data Intranet = Intranet { 
    getYahooPortfolioManagerSite :: YahooPortfolioManagerSite,
    getNotebook :: Notebook,
    connPool    :: ConnectionPool, 
    httpManager :: Manager
}

mkYesodData "Intranet" $(parseRoutesFile "config/routes")

instance Yesod Intranet where
    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance YesodPersist Intranet where 
    type YesodPersistBackend Intranet = SqlBackend 
    runDB f = do 
        master <- getYesod 
        let pool = connPool master 
        runSqlPool f pool

instance RenderMessage Intranet FormMessage where
    renderMessage _ _ = defaultFormMessage

--instance Yesod Intranet
instance YesodNic Intranet
