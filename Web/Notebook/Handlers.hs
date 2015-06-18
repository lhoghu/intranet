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

-- Form to create new notes
noteForm :: ( Yesod master
            , RenderMessage master FormMessage
            , YesodNic master) =>
            Html -> 
            MForm (HandlerT master IO) 
                  ( FormResult Note
                  , (WidgetT (HandlerSite (HandlerT master IO)) IO ()))
noteForm = renderDivs $ Note
    <$> areq textField (fieldSettingsLabel ("Title" :: Text)) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> areq nicHtmlField (fieldSettingsLabel ("Content" :: Text)) Nothing
-- noteForm extra = lift $ do
--     (titleRes, titleView) <- mreq textField "this is not used" Nothing
--     (timeRes, timeView) <- (getCurrentTime)
--     (contentRes, contentView) <- mreq nicHtmlField "neither is this" Nothing
--     let noteRes = Note <$> titleRes <*> timeRes <*> contentRes
--     let widget = do
--                     [whamlet|
--                         #{extra}
--                         <div>
--                             <p> Title
--                             ^{fvInput titleView}
--                         <div>
--                             <p> Content
--                             ^{fvInput contentView}
--                     |]
--     return (noteRes, widget)

-- Form to create new comments
commentForm :: ( Yesod master
               , RenderMessage master FormMessage
               , YesodNic master) =>
               NoteId -> 
               Html ->
               MForm (HandlerT master IO) 
                     ( FormResult Comment
                     , (WidgetT (HandlerSite (HandlerT master IO)) IO ()))
commentForm noteId = renderDivs $ Comment
    <$> pure noteId
    <*> lift (liftIO getCurrentTime)
    <*> areq textareaField (fieldSettingsLabel ("Comment" :: Text)) Nothing

-- Notebook homepage
getNotebookHomeR :: Yesod master => HandlerT Notebook (HandlerT master IO) Html
getNotebookHomeR = do
    toParent <- getRouteToParent
    lift $ defaultLayout $ do
        setTitle "Notebook - Home"
        toWidget $(hamletFile "Web/Notebook/templates/notebookHome.hamlet")

-- Display a list of the notes stored in the db
getNotebookBlogR :: (Yesod master, RenderMessage master FormMessage, YesodNic master) =>
                    YesodPersist master =>
                    YesodPersistBackend master ~ SqlBackend => 
                    HandlerT Notebook (HandlerT master IO) Html
getNotebookBlogR = do
    toParent <- getRouteToParent
    
    -- retrieve notes, most recent first
    notes <- lift $ runDB $ selectList [] [Desc NotePosted]
    (note, enctype) <- lift $ generateFormPost noteForm
    
    pc <- lift $ widgetToPageContent note
    let noteWidget = pageBody pc

    lift $ defaultLayout $ do
        setTitle "Notebook"
        toWidget $(hamletFile "Web/Notebook/templates/notebookBlog.hamlet")

-- Add a new note to the db
postNotebookBlogR :: ( Yesod master
                     , RenderMessage master FormMessage
                     , YesodNic master) =>
                    YesodPersist master =>
                    YesodPersistBackend master ~ SqlBackend => 
                    HandlerT Notebook (HandlerT master IO) Html
postNotebookBlogR = do
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

-- Display a single note along with the associated comments, and provide
-- a form to add new comments
getNoteR :: (Yesod master, RenderMessage master FormMessage, YesodNic master) =>
                    YesodPersist master =>
                    YesodPersistBackend master ~ SqlBackend => 
                    NoteId ->
                    HandlerT Notebook (HandlerT master IO) Html
getNoteR noteId = do
    toParent <- getRouteToParent
    (note, comments) <- lift $ runDB $ do
        note <- get404 noteId
        
        -- Show comments, oldest first
        comments <- selectList [CommentNote ==. noteId] [Asc CommentPosted]
        return (note, map entityVal comments)
    (comment, enctype) <- lift $ generateFormPost (commentForm noteId)

    pc <- lift $ widgetToPageContent comment
    let commentWidget = pageBody pc

    lift $ defaultLayout $ do
        setTitle $ toHtml $ noteTitle note
        toWidget $(hamletFile "Web/Notebook/templates/note.hamlet")

-- Add a new comments
postNoteR :: (Yesod master, RenderMessage master FormMessage, YesodNic master) =>
                    YesodPersist master =>
                    YesodPersistBackend master ~ SqlBackend => 
                    NoteId ->
                    HandlerT Notebook (HandlerT master IO) Html
postNoteR noteId = do
    ((res, commentWidget), enctype) <- lift $ runFormPost (commentForm noteId)
    case res of
        FormSuccess comment -> do
            _ <- lift $ runDB $ insert comment
            setMessage "Comment added"
            redirect $ NoteR noteId
            lift $ defaultLayout $ do
                setTitle "There was an error with the comment submission, please try again"
                [whamlet|
                    <form method=post enctype=#{enctype}>
                    ^{commentWidget}
                    <div>
                        <input type=submit value="Add comment">
                |]
