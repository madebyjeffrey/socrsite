{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes #-}
module Handler.Document where

import Import

import Text.Markdown (markdown, def)
import qualified Data.Text.Lazy as TL

errorPage :: forall (backend :: (* -> *) -> * -> *). DocumentGeneric backend
errorPage = Document "Document not found" "" "No document by that name was found" "" ""

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getDocumentR :: Text -> Handler RepHtml
getDocumentR pageName = do
    s <- runDB $ selectList [DocumentSlug ==. pageName] []
    
    let Document articleTitle slug content author processor = case s of
                        [] -> errorPage
                        (Entity _ value):_ -> value
        handlerName = "getDocumentR" :: Text
        renderedContent = toWidget $ markdown def $ TL.fromStrict content
    defaultLayout $ do
        setTitle $ toHtml articleTitle
        $(widgetFile "document")



--        
