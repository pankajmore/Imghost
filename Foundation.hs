{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings, FlexibleContexts, TypeSynonymInstances #-}
module Foundation 
    ( ImgHost(..)
    , ImgHostRoute (..)
    , resourcesImgHost
    , openConnectionCount
    , uploadDirectory
    , sUploadDirectory
    , defaultTags
    , Widget
    , Handler
    , module Settings
    , module Settings.StaticFiles
    , module Yesod
    , module Yesod.Goodies
    , module Models
    , module Yesod.Static
    , module Yesod.Auth
    )where
import Settings.StaticFiles
import Settings
import Yesod
import Models
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Yesod.Static
import Database.Persist.Sqlite
import Text.Hamlet (hamletFile)
import Yesod.Comments hiding (userName, userEmail)
import Yesod.Comments.Management
import Yesod.Comments.Storage
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.Auth.Facebook
import Yesod.Auth.BrowserId
import Yesod.Goodies hiding (NotFound)
import Data.Maybe (fromMaybe)
import Web.ClientSession (getKey)
import Control.Applicative
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logLazyText)
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data ImgHost = ImgHost 
        { settings :: AppConfig DefaultEnv
        , getLogger :: Logger
        , getStatic :: Static -- ^ Settings for static file serving.
        , connPool  :: ConnectionPool
        }
mkYesodData "ImgHost" $(parseRoutesFile "routes")
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
instance Yesod ImgHost where
    approot _ = "http://localhost:5432" --change this to website domain-name other openid wont work
    authRoute _ = Just $ AuthR LoginR
    encryptKey _ = fmap Just $ getKey "client_session_key.aes"
    maximumContentLength _ (Just (UploadR {})) = 32 * 1024 * 1024
    maximumContentLength _ _ = 2 * 1024 * 1024
    defaultLayout widget = do
        mmsg <- getMessage
        muid <- maybeAuth
        let mgrav = fmap getGravatar muid
        pc <- widgetToPageContent $ do
            $(widgetFile "header")
            $(widgetFile "normalize")
            $(widgetFile "default-layout")
            $(widgetFile "footer")
        hamletToRepHtml $(hamletFile "hamlet/default-layout-wrapper.hamlet")

        where
            getGravatar :: (UserId, User) -> String
            getGravatar (_,u) = let email = fromMaybe "" $ userEmail u
                                in  gravatarImg email gravatarOpts

            gravatarOpts :: GravatarOptions
            gravatarOpts = defaultOptions
                { gSize    = Just $ Size 12
                , gDefault = Just MM
                }


instance RenderMessage ImgHost FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist ImgHost where
    type YesodPersistBackend ImgHost = SqlPersist
    runDB action = liftIOHandler $ connPool <$> getYesod >>= runSqlPool action

instance YesodAuth ImgHost where
    type AuthId ImgHost = UserId

    loginDest _ = ProfileR
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueIdent $ credsIdent creds
        case x of
            Just (_, i) -> do
                return $ Just $ identUser i

            Nothing -> do
                uid <- insert $ User Nothing Nothing False
                _   <- insert $ Ident (credsIdent creds) uid
                return $ Just uid

    authPlugins = [ authOpenId, authBrowserId']

    loginHandler = defaultLayout $ do
        setTitle "Login"
        addWidget $(widgetFile "login")


instance YesodComments ImgHost where
    getComment     = getCommentPersist
    storeComment   = storeCommentPersist
    updateComment    = updateCommentPersist
    deleteComment  = deleteCommentPersist
    loadComments   = loadCommentsPersist
    displayUser  uid = maybe' "anonymous" userName  =<< runDB (get uid)
    displayEmail uid = maybe' ""          userEmail =<< runDB (get uid)

maybe' :: Monad m => b -> (a -> Maybe b) -> Maybe a -> m b
maybe' c f = return . fromMaybe c . maybe Nothing f

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
openConnectionCount :: Int
openConnectionCount = 10
uploadDirectory :: Text
uploadDirectory = "./static/upload/"
sUploadDirectory :: Text
sUploadDirectory = T.tail uploadDirectory
defaultTags :: [(Text,Text)]
defaultTags = [("Nature","Nature"),("Celebrity","Celebrity"),("Machines","Machines"),("NSFW","NSFW"),("Other","Other")]

