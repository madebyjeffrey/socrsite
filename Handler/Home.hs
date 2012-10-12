{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Yesod.Auth
import Web.Authenticate.BrowserId

import Import

import Text.Blaze.Internal

import System.Directory
import qualified Data.Text.IO as TIO

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    credentials <- maybeAuthId
   
    newsitems <- runDB $ selectList [] [Desc NewsItemDate]
    
    let pagename = "/usr/local/socr/pages/index"
    exists <- liftIO $ doesFileExist pagename
    
    index <- case exists of
                True -> liftIO $ TIO.readFile pagename
                False -> return ""
    
    let handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
--        tm <- lift getRouteToMaster
--        let n =  (apLogin browserid) tm
        setTitle "Student Operated Computing Resources"
        addScriptRemote browserIdJs
        $(widgetFile "homepage") 

-- removed login from default-layout: ^{login curauth}
