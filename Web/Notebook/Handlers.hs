{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Web.Notebook.Handlers where

import Control.Applicative
import Control.Monad.Trans (liftIO) 
import Data.Text (Text, pack)
import Text.Hamlet (hamletFile)
import Text.Printf
import Web.Notebook.Foundation 
import Yesod


-- notebookLayout :: Yesod master => Html
--                                -> HtmlUrl (Route master)
--                                -> HandlerT Notebook (HandlerT master IO) Html
-- notebookLayout title widget = do
--     -- Need to lift the child site url to the parent site in order
--     -- to create links within the subsite: see use of toParent in 
--     -- sidebar.hamlet (http://www.yesodweb.com/blog/2013/03/big-subsite-rewrite)
--     toParent <- getRouteToParent
--     let content = widget 
--     lift $ defaultLayout $ do
--         setTitle title

getNotebookHomeR :: Yesod master => HandlerT Notebook (HandlerT master IO) Html
getNotebookHomeR = do
    toParent <- getRouteToParent
    lift $ defaultLayout $ do
        setTitle "Notebook - Home"
        toWidget $(hamletFile "Web/Notebook/templates/notebookHome.hamlet")

getNotebookBlogR :: Yesod master => HandlerT Notebook (HandlerT master IO) Html
getNotebookBlogR = do
    toParent <- getRouteToParent
    lift $ defaultLayout $ do
        setTitle "Notebook"
        toWidget $(hamletFile "Web/Notebook/templates/notebookBlog.hamlet")
