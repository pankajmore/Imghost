{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Forms.Profile( runProfileFormGet
                    , runProfileFormPost) where

import Foundation
import Yesod 
import Data.Text (Text)
import Control.Applicative
import Data.Time           (getCurrentTime)
import Yesod.Auth

data ProfileEditForm = ProfileEditForm
    { formUsername :: Maybe Text
    , formEmail    :: Maybe Text
    }

-- | Display the form for user input
runProfileFormGet :: Widget
runProfileFormGet = do
    (_, u)               <- lift requireAuth
    ((_, form), enctype) <- lift $ runFormPost $ profileEditForm u
    [whamlet|
        <h1>Edit
        <article .fullpage .profile
            <form enctype="#{enctype}" method="post">
                <table>
                    ^{form}
                    <tr>
                        <td>&nbsp;
                        <td .buttons>
                            <input type="submit" value="Save">
        |]

-- | Handle the POST request. This must be a separate call since all 
--   fields are optional so an empty for would be constantly POSTed if 
--   we consolidated the code.
runProfileFormPost :: Handler ()
runProfileFormPost = do
    (uid, u)          <- requireAuth
    ((res, _   ), _ ) <- runFormPost $ profileEditForm u
    case res of
        FormSuccess ef -> saveChanges uid ef
        _              -> return ()

    where
        saveChanges :: UserId -> ProfileEditForm -> Handler ()
        saveChanges uid ef = do
            runDB $ update uid 
                [ UserName  =. formUsername ef
                , UserEmail =. formEmail    ef
                ]

            tm <- getRouteToMaster
            redirect RedirectTemporary $ tm ProfileR

profileEditForm :: User -> Html -> Form ImgHost ImgHost (FormResult ProfileEditForm, Widget)
profileEditForm u = renderTable $ ProfileEditForm
    <$> aopt textField   "User name"
        { fsTooltip = Just "comments are attributed to this username"
        } (Just $ userName u)

    <*> aopt emailField  "Email address"
        { fsTooltip = Just "never displayed, only used to find your gravatar"
        } (Just $ userEmail u)


