{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Helpers.Storage where
import Foundation
import Control.Applicative 
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson 

toSqlImage :: Image -> SqlImage
toSqlImage img = SqlImage
    { sqlImageName = name img
    , sqlImageTag = tag img
    , sqlImageOwner = owner img
    , sqlImageCaption = caption img 
    , sqlImageVotes = votes img
    , sqlImageCreated = creadted img
    }
    
fromSqlImage :: Yesod y => SqlImage -> Image
fromSqlImage y img = Image
    { name  = sqlImageName img
    , tag = sqlImageTag Img
    , owner = sqlImageOwner img 
    , caption = sqlImageCaption img
    , votes = sqlImageVotes img
    , created = sqlImageCreated img
    }

-- get an Image by its id from the database
getImagePersist :: (YesodPersist m , PersistBackend ( YesodPersistBackend m) (GGHandler s m IO)) => SqlImageId -> GHandler s m (Maybe Image)
getImagePersist id  = return . fmap (fromSqlImage . snd) =<< runDB $ get id

-- stores the given Image In the database
storeImagePersist :: (YesodPersist m , PersistBackend ( YesodPersistBackend m) (GGHandler s m IO)) => Image -> GHandler s m ()
storeImagePersist image = return . const () =<< runDB (insert $ toSqlImage image)

-- deletes the given Image from the database
deleteImagePersist :: (YesodPersist m , PersistBackend ( YesodPersistBackend m) (GGHandler s m IO)) => Image -> GHandler s m ()
deleteImagePersist image = return . const () =<< runDB (deleteBy $ UniqueName (name image))

-- Updates only caption and votes 
updateImagePersist :: (YesodPersist m, PersistBackend (YesodPersistBackend m) (GGHandler s m IO)) => Comment -> Comment -> GHandler s m ()
updateImagePersist old new = do
    mres <- runDB (getBy $ UniqueName (name old))
    case mres of
        Just (k,_) -> runDB $ update k [SqlImageVotes =. (votes new),SqlImageCaption =. (caption new) ]
        _          -> return ()



instance ToJSON Image where 
    toJSON image = object [ "src" .= name image
                          , "name" .= caption image
                          , "tag" .= tag image
                          , "owner" .= owner image
                          , "votes" .= votes image 
                          , "created" .= created image
                          ]

