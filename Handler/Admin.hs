{-# LANGUAGE OverloadedStrings #-}
module Handler.Admin where

import Import
import Yesod.Auth


{-

profileForm :: Html -> MForm App App (FormResult User, Widget)
profileForm :: renderDivs $ User
    <$> areq textField "Name" Nothing
    <*> areq textField "Email address" Nothing
    <*> areq textField "Access Level" Nothing
-}
getAdminProfileR :: Handler RepHtml
getAdminProfileR = do
    credentials <- requireAuthId
    defaultLayout $ do
        $(widgetFile "adminprofile")

