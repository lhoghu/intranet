{-# LANGUAGE OverloadedStrings #-}

import Dispatch ()
import Foundation
import Web.Notebook.App
import Web.YahooPortfolioManager.App
import Yesod
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Conduit (newManager)
import Database.Persist.Sqlite
    (runMigration, createSqlitePool, runSqlPersistMPool)
import Control.Monad.Logger (runStdoutLoggingT)

main :: IO ()
main = do
    -- create a new pool
    pool <- runStdoutLoggingT $ createSqlitePool "notebook.db" 10 
    
    -- perform any necessary migration
    runSqlPersistMPool (runMigration migrateNotebook) pool

    -- create a new HTTP manager
    manager <- newManager tlsManagerSettings 

    warp 3000 $ Intranet YahooPortfolioManagerSite Notebook pool manager
