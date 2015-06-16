{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Notebook.Foundation where

import Yesod

data Notebook = Notebook 

mkYesodSubData 
    "Notebook" 
    $(parseRoutesFile "Web/Notebook/config/routes")
