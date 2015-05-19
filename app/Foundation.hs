{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import Settings
import Text.Hamlet (hamletFile)
import Yesod

data Intranet = Intranet 

mkYesodData "Intranet" $(parseRoutesFile "config/routes")

instance Yesod Intranet where
    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
