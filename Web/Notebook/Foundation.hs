{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE QuasiQuotes                   #-}
{-# LANGUAGE ViewPatterns                  #-}

module Web.Notebook.Foundation where

import Data.Text (Text)
import Yesod
import Yesod.Form.Nic (YesodNic)
import Network.HTTP.Conduit (Manager, newManager)
import Database.Persist.Sqlite
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration
    , createSqlitePool, runSqlPersistMPool
    )
import Data.Time (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateNotebook"] [persistLowerCase|
    Note
        title Text
        posted UTCTime
        content Html
|]

data Notebook = Notebook {
    -- connPool    :: ConnectionPool, 
    -- httpManager :: Manager
}

-- instance YesodPersist Notebook where 
--     type YesodPersistBackend Notebook = SqlBackend 
--     runDB f = do 
--         master <- getYesod 
--         let pool = connPool master 
--         runSqlPool f pool

-- instance RenderMessage Notebook FormMessage where
--     renderMessage _ _ = defaultFormMessage

mkYesodSubData 
    "Notebook" 
    $(parseRoutesFile "Web/Notebook/config/routes")

-- instance Yesod Notebook
-- instance YesodNic Notebook
