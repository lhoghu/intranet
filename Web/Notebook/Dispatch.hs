{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ViewPatterns          #-}

module Web.Notebook.Dispatch where

import Database.Persist.Sqlite (SqlBackend)
import Text.Hamlet (hamletFile)
import Web.Notebook.Foundation 
import Web.Notebook.Handlers
import Yesod
import Yesod.Form.Nic (YesodNic)

instance ( Yesod master
         , RenderMessage master FormMessage
         , YesodNic master
         , YesodPersist master
         , YesodPersistBackend master ~ SqlBackend) => 
         YesodSubDispatch Notebook (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesNotebook)
