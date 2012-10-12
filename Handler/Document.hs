{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes #-}
module Handler.Document where

import Import

import System.Directory


import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Blaze.Internal  

errorPage :: forall (backend :: (* -> *) -> * -> *). DocumentGeneric backend
errorPage = Document "Document not found" "" "No document by that name was found" "" ""

getDocumentR :: Text -> Handler RepHtml
getDocumentR pageName = do
    liftIO $ putStrLn $ show (T.breakOnEnd "." pageName)
    
    
    case T.breakOnEnd "." pageName of 
        (_, "css") -> do
            bareLayout ("/usr/local/socr/pages/" <> T.unpack pageName)
        _ -> do
            s <- runDB $ selectList [DocumentSlug ==. pageName] []
    
            let pagename = ("/usr/local/socr/pages/" <> T.unpack pageName)
                cssname = ("/usr/local/socr/pages/" <> T.unpack pageName <> ".css")
    
            -- content comes from this path
            content <- liftIO $ TIO.readFile pagename
            hasCss <- liftIO $ doesFileExist cssname
            let Document articleTitle slug _ author processor = case s of
                                [] -> errorPage
                                (Entity _ value):_ -> value
                handlerName = "getDocumentR" :: Text
        --        renderedContent = toWidget $ markdown def $ TL.fromStrict content 
                renderedContent = content
            defaultLayout $ do 
                setTitle $ toHtml articleTitle
                $(widgetFile "document")
                if (hasCss) 
                    then addStylesheetRemote ("/page/" <> pageName <> ".css")
                    else return ()






