{-# LANGUAGE OverloadedStrings #-}
module Handler.AdminData where

import Import hiding (object)


import Yesod.Auth
import Data.Aeson




getAdminDataProfileR :: Text -> Handler RepJson
getAdminDataProfileR email = do
    credentials <- requireAuthId

    user <- runDB $ getBy (UniqueUser email)
    
    let js = case user of 
                Nothing -> object [ "valid" .= False ]
                Just (Entity id' value) -> object [ "valid" .= True
                                                  , "id" .= id'
                                                  , "email" .= userEmail value
                                                  , "name" .= userName value
                                                  , "access" .= userAccess value
                                                  ]
                                         
    jsonToRepJson js
    
postAdminDataProfileR :: Text -> Handler RepJson
postAdminDataProfileR _ = do
    $(logInfo) "received json request"
    credentials <- requireAuthId
    (data' :: User) <- parseJsonBody_ 
    $(logInfo) "parse OK"
    tid <- runDB $ updateWhere [ UserEmail ==. credentials, UserEmail ==. (userEmail data') ] [ UserName =. (userName data') ]

    jsonToRepJson $ object [ "success" .= True ]

/*        tid <- runDB $ insert (todo :: Todo)
        jsonToRepJson $ object ["status" .= ("post ok" :: Text)]
  */  
    
    
    
getAdminNewsCountR :: Handler RepJson
getAdminNewsCountR = do
    credentials <- requireAuthId
    
    newscount <- runDB $ count [ NewsItemDate !=. thatTime ]
    
    jsonToRepJson $ object [ "count" .= newscount ]
    
getAdminNewsListR :: Handler RepJson
getAdminNewsListR = jsonToRepJson $ array ([]::[Int])

getAdminNewsItemR :: Int -> Handler RepJson
getAdminNewsItemR key = jsonToRepJson $ array ([]::[Int])

postAdminNewsItemR :: Int -> Handler RepJson
postAdminNewsItemR key = jsonToRepJson $ array ([]::[Int])

{-
    creates a new news item
-}
putAdminNewsR :: Handler RepJson
putAdminNewsR = do
    credentials <- requireAuthId
    
    (data' :: NewsItem) <- parseJsonBody_
    
    runDB $ insert data'
    
    jsonToRepJson $ object [ "success" .= True ]


getAdminPagesCountR :: Handler RepJson
getAdminPagesCountR = jsonToRepJson $ array ([]::[Int])

getAdminPagesListR :: Handler RepJson
getAdminPagesListR = jsonToRepJson $ array ([]::[Int])

getAdminPagesItemR :: Int -> Handler RepJson
getAdminPagesItemR _ = jsonToRepJson $ array ([]::[Int])

postAdminPagesItemR :: Int -> Handler RepJson
postAdminPagesItemR _ = jsonToRepJson $ array ([]::[Int])

putAdminPagesR :: Handler RepJson
putAdminPagesR = jsonToRepJson $ array ([]::[Int])
