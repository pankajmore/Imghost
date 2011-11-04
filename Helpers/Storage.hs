{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Helpers.Storage where
import Foundation
import Helpers.Document
import Control.Applicative 
import qualified Data.Text as T
import Data.Aeson 
import System.Random
import Directory (removeFile)
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

getImageByName :: (YesodPersist m , PersistBackend ( YesodPersistBackend m) (GGHandler s m IO)) => Text -> GHandler s m (Maybe Image)
getImageByName name   = return . fmap (fromSqlImage . snd) =<< runDB (getBy $ UniqueName name )

-- stores the given Image In the database
storeImagePersist :: (YesodPersist m , PersistBackend ( YesodPersistBackend m) (GGHandler s m IO)) => Image -> GHandler s m SqlImageId
storeImagePersist image = runDB (insert $ toSqlImage image)

-- deletes the given Image from the database
deleteImagePersist :: (YesodPersist m , PersistBackend ( YesodPersistBackend m) (GGHandler s m IO)) => Image -> GHandler s m ()
deleteImagePersist image = return . const () =<< runDB (deleteBy $ UniqueName (name image))

deleteImage :: (YesodPersist m , PersistBackend ( YesodPersistBackend m) (GGHandler s m IO)) => SqlImageId -> GHandler s m ()
deleteImage id = do
    image <- getImagePersist id
    case iName of
        Just name -> do 
            let thumbnail = getThumb name 
            liftIO $ removeFile (uploadDirectory ++ thumbnail)
            liftIO $ removeFile (uploadDirectory ++ name)
            deleteImagePersist image
        _ -> setMessage "Image not Found"


-- Updates only caption and votes 
updateImagePersist :: (YesodPersist m, PersistBackend (YesodPersistBackend m) (GGHandler s m IO)) => Comment -> Comment -> GHandler s m ()
updateImagePersist old new = do
    mres <- runDB (getBy $ UniqueName (name old))
    case mres of
        Just (k,_) -> runDB $ update k [SqlImageVotes =. (votes new),SqlImageCaption =. (caption new) ]
        _          -> return ()

updateById :: (YesodPersist m, PersistBackend (YesodPersistBackend m) (GGHandler s m IO),PersistEntity val) => Key (YesodPersistBackend m) val -> [Update val]-> GHandler s m ()
updateById id updateField = runDB $ update id updateField

-- Get By tag 
getImageByTag :: (YesodPersist m, PersistBackend (YesodPersistBackend m) (GGHandler s m IO)) => Text -> Int -> Int -> GHandler s m [(SqlImageId , Image)]
getImageByTag t count offset = map getPair <$> runDB (selectList 
                                [ ImagesImageTag ==. tagquery]
                                [ Desc ImagesCreated
                                , LimitTo count 
                                , OffsetBy offset
                                ])
 where 
    getPair (a,b) = (a, fromSqlImage b) 

data JsonImage = JsonImage 
    { jName :: Text
    , jLink :: Text
    , jSrc :: Text
    , jTag :: Text
    }
toJsonImage :: Text -> Image -> JsonImage 
toJsonImage lnk image = JsonImage 
    { jName = caption image 
    , jLink = lnk
    , jSrc = T.append sUploadDirectory name 
    , jTag = tag image 
    }
instance ToJSON JsonImage where 
    toJSON image = object [ "src" .= jSrc image
                          , "name" .= jName image
                          , "tag" .= jTag image
                          , "link" .= jLink image
                          ]
---------------------------------------------------------------------------------------------------
--Votes Storage Functions 
-- get an Image by its id from the database
getVotesPersist :: (YesodPersist m , PersistBackend ( YesodPersistBackend m) (GGHandler s m IO)) => UserId -> SqlImageId -> GHandler s m (Maybe (VotesId,Votes))
getVotePersist uid id  = runDB.getBy $ UniqueVote uid id


-- stores the given Vote In the database
storeVotesPersist :: (YesodPersist m , PersistBackend ( YesodPersistBackend m) (GGHandler s m IO)) => Votes -> GHandler s m ()
storeVotesPersist vote = return . const () =<< runDB (insert vote)

---------------------------------------------------------------------------------------------------
{-
 - Generates a random name of length 20 containing chars from a-z, checks if
 - the database already contains that name, if calls itself again to do another
 - generation and check until it finds a unique name.
-}
getRandomName :: Text -> GHandler ImgHost ImgHost Text
getRandomName ending = do  
                    gen <- liftIO getStdGen  
                    let a = take 20 (randomRs ('a','z') gen) 
                    liftIO $ newStdGen
                    let isInDatabase = False
                    let name = T.append a ending
                    isInDatabase <- checkDatabase name
                    if (not isInDatabase)
                        then return name -- return name.ext
                        else getRandomName ending

checkDatabase :: Text -> GHandler ImgHost ImgHost Bool
checkDatabase image = do 
           maybeImage <- getImageByName image  
           case maybeImage of
               Just _ -> return True
               _ -> return False

getOwner id = do
    mayBeImage <- getImageById id  
    return $ fmap owner maybeImage

--requireAdmin :: Handler ()
requireAdmin ownerid = do
    currentid <- requireAuthId
    case ownerid of 
        Nothing -> permissionDenied "Image not found"
        Just (Nothing) -> permissionDenied "This image is orphan"
        Just (Just x) -> if currentid == x then return () else permissionDenied "You are not authorized to touch this!!"

canIVote :: UserId -> SqlImageId -> Handler Bool
canIVote uid id = do 
    alreadyInVotes <- getVotesPersist uid id 
    case alreadyInVotes of
        Nothing -> return True
        Just (qid , val) -> 
            case votesValue val of
                1 -> return False 
                _ -> return True

canDVote :: UserId -> SqlImageId -> Handler Bool
canDVote uid id = do 
    alreadyInVotes <- getVotesPersist uid id 
    case alreadyInVotes of
        Nothing -> return True
        Just (qid , val) -> 
            case votesValue val of
                (-1) -> return False 
                _ -> return True

canDeleteImage :: UserId -> SqlImageId -> Handler Bool
canDeleteImage uid id = do
            ownerid <- getOwner id
            case ownerid of
                Nothing -> return False
                Just Nothing -> return False
                Just (Just oid) -> if uid == oid then return True else return False

