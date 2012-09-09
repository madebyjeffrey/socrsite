{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Yesod.Auth
import Yesod.Auth.BrowserId
import Web.Authenticate.BrowserId

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    maid <- maybeAuthId
    let browserid = authBrowserId
    
    newsitems <- runDB $ selectList [] [Desc NewsItemDate]
    let handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
--        tm <- lift getRouteToMaster
--        let n =  (apLogin browserid) tm
        setTitle "Student Operated Computing Resources"
        addScriptRemote browserIdJs
        $(widgetFile "homepage")

