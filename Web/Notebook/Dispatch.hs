{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Web.Notebook.Dispatch where

import Text.Hamlet (hamletFile)
import Web.Notebook.Foundation 
import Web.Notebook.Handlers
import Yesod

instance Yesod master => YesodSubDispatch Notebook (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesNotebook)
