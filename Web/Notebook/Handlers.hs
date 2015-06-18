{-# LANGUAGE FlexibleInstances             #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE QuasiQuotes                   #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE FlexibleContexts              #-}

module Web.Notebook.Handlers where

import Control.Applicative
import Control.Monad.Trans (liftIO) 
import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import Database.Persist.Sqlite (SqlBackend)
import Text.Hamlet (hamletFile)
import Text.Printf
import Web.Notebook.Foundation 
import Yesod
import Yesod.Form.Nic (YesodNic, nicHtmlField)


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

noteForm :: (Yesod master, RenderMessage master FormMessage, YesodNic master) =>
            Html -> 
            MForm (HandlerT master IO) 
                  (FormResult Note, (WidgetT (HandlerSite (HandlerT master IO)) IO ()))
noteForm = renderDivs $ Note
    <$> areq textField (fieldSettingsLabel ("Title" :: Text)) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> areq nicHtmlField (fieldSettingsLabel ("Content" :: Text)) Nothing

getNotebookHomeR :: Yesod master => HandlerT Notebook (HandlerT master IO) Html
getNotebookHomeR = do
    toParent <- getRouteToParent
    lift $ defaultLayout $ do
        setTitle "Notebook - Home"
        toWidget $(hamletFile "Web/Notebook/templates/notebookHome.hamlet")

getNotebookBlogR :: (Yesod master, RenderMessage master FormMessage, YesodNic master) =>
                    YesodPersist master =>
                    YesodPersistBackend master ~ SqlBackend => 
                    HandlerT Notebook (HandlerT master IO) Html
getNotebookBlogR = do
    toParent <- getRouteToParent
    
    -- retrieve notes, most recent first
    notes <- lift $ runDB $ selectList [] [Desc NotePosted]
    (noteWidget, enctype) <- lift $ generateFormPost noteForm
    lift $ defaultLayout $ do
        setTitle "Notebook"
        [whamlet|
        <h1> Notes
        $if null notes
            <p> No notes have been recorded
        $else
            <ul>
                $forall Entity noteId note <- notes
                    <li>
                        <a href=@{toParent $ NoteR noteId} > #{noteTitle note}
        <form method=post enctype=#{enctype}>
            ^{noteWidget}
            <div>
                <input type=submit value="Create new note">
        |]
        --toWidget $(hamletFile "Web/Notebook/templates/notebookBlog.hamlet")

postNotebookBlogR :: ( Yesod master
                     , RenderMessage master FormMessage
                     , YesodNic master) =>
                    YesodPersist master =>
                    YesodPersistBackend master ~ SqlBackend => 
                    HandlerT Notebook (HandlerT master IO) Html
postNotebookBlogR = do
    --toParent <- getRouteToParent
    ((res, noteWidget), enctype) <- lift $ runFormPost noteForm
    case res of
        FormSuccess note -> do
            noteId <- lift $ runDB $ insert note
            setMessage $ "Note added"
            redirect $ NoteR noteId
            lift $ defaultLayout $ do
                setTitle "There were errors during submission, please try again"
                [whamlet|
                    <form method=post enctype=#{enctype}>
                    ^{noteWidget}
                    <div>
                        <input type=submit value="Add note">
                |]

getNoteR :: (Yesod master, RenderMessage master FormMessage, YesodNic master) =>
                    YesodPersist master =>
                    YesodPersistBackend master ~ SqlBackend => 
                    NoteId ->
                    HandlerT Notebook (HandlerT master IO) Html
getNoteR noteId = do
    toParent <- getRouteToParent
    (note) <- lift $ runDB $ do --(note, comments) <- runDB $ do
        note <- get404 noteId
        --comments <- selectList [CommentEntry ==. entryId] [Asc CommentPosted]
        return (note) --, map entityVal comments)
    lift $ defaultLayout $ do
        setTitle $ toHtml $ noteTitle note
        [whamlet|
            <h2>#{noteTitle note}
            <article>#{noteContent note}
            <a href=@{toParent NotebookBlogR}>Back to notes
            |]
        --     <section .comments>
        --         <h1>_{MsgCommentsHeading}
        --         $if null comments
        --             <p> No comments
        --         $else
        --             $forall Comment _entry posted _user name text <- comments
        --                 <div .comment>
        --                     <span .by>#{name}
        --                     <span .at>#{show posted}
        --                     <div .content>#{text}
        --                     <section>
        --                         <h3>Add comment
        --                             <form method=post enctype=#{enctype}>
        --                             ^{commentWidget}
        --                             <div>
        --                                 <input type=submit value="Add comment">
        -- |]

postNoteR :: (Yesod master, RenderMessage master FormMessage, YesodNic master) =>
                    YesodPersist master =>
                    YesodPersistBackend master ~ SqlBackend => 
                    NoteId ->
                    HandlerT Notebook (HandlerT master IO) Html
postNoteR = undefined
