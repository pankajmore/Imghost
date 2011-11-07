{-# LANGUAGE FlexibleContexts #-}
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
    , sqlImageTags = tags img
    , sqlImageOwner = owner img
    , sqlImageCaption = caption img 
    , sqlImageVotes = votes img
    , sqlImageHits = hits img
    , sqlImageCreated = created img
    }
    
fromSqlImage :: SqlImage -> Image
fromSqlImage img = Image
    { name  = sqlImageName img
    , tags = sqlImageTags img
    , owner = sqlImageOwner img 
    , caption = sqlImageCaption img
    , votes = sqlImageVotes img
    , hits = sqlImageHits img
    , created = sqlImageCreated img
    }

-- get an Image by its id from the database
getImagePersist :: (YesodPersist m , PersistBackend ( YesodPersistBackend m) (GGHandler s m IO)) => Key (YesodPersistBackend m) SqlImage-> GHandler s m (Maybe Image)
getImagePersist id  = do 
                    maybeSqlImage <- runDB $ get id
                    return $ fmap fromSqlImage maybeSqlImage 

getImageByName :: (YesodPersist m , PersistBackend ( YesodPersistBackend m) (GGHandler s m IO)) => Text -> GHandler s m (Maybe Image)
getImageByName name   = do 
                         maybePair <- runDB (getBy $ UniqueName name )
                         return $ fmap (fromSqlImage . snd) maybePair 

-- stores the given Image In the database
storeImagePersist :: (YesodPersist master , PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) => Image-> GHandler sub master (Key (YesodPersistBackend master) SqlImage)
storeImagePersist image = do 
    id <- runDB (insert $ toSqlImage image)
    storeTags (T.splitOn "," $ tags image) (created image) id
    return id

-- stores the relation ship with tags in the tag table 
storeTags :: (YesodPersist master , PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) => [Text] -> UTCTime -> Key backend SqlImage-> GHandler sub master ()
storeTags [] ctime id = return ()
storeTags (x:xs) ctime id = do
    runDB (insert $ Tag id x ctime )
    storeTags xs ctime id 
    return ()
-- deletes the given Image from the database
deleteImagePersist :: (YesodPersist master , PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) => Image -> GHandler sub master ()
deleteImagePersist image = return . const () =<< runDB (deleteBy $ UniqueName (name image))

{-deleteTags :: (YesodPersist master , PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) =>  -> GHandler sub master ()-}
deleteTags id = return . const () =<< runDB (deleteWhere [TagImageId ==. id])

{-deleteImage :: (YesodPersist master , PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) => Key (YesodPersistBackend master) SqlImage-> GHandler sub master ()-}
deleteImage id = do
    maybeImage <- getImagePersist id
    case maybeImage of
        Just image -> do 
            let thumbnail = getThumb $ name image 
            liftIO.removeFile $ T.unpack (T.append uploadDirectory thumbnail)
            liftIO.removeFile $ T.unpack (T.append uploadDirectory  (name image))
            deleteImagePersist image
            deleteTags id 
        _ -> setMessage "Image not Found"


-- Updates only caption and votes 
updateImagePersist :: (YesodPersist master , PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) => Image -> Image -> GHandler sub master ()
updateImagePersist old new = do
    mres <- runDB (getBy $ UniqueName (name old))
    case mres of
        Just (k,_) -> runDB $ update k [SqlImageVotes =. (votes new),SqlImageCaption =. (caption new) ]
        _          -> return ()

updateById :: (YesodPersist master , PersistEntity val , PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) => Key (YesodPersistBackend master) val-> [Update val]-> GHandler sub master ()
updateById id updateField = runDB $ update id updateField

-- Get By tag 
{-getImageByTag :: (YesodPersist master,PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) => Maybe Text-> Maybe Int-> Int-> GHandler sub master [(Key (YesodPersistBackend master) SqlImage,Image)]-}
{-getImageByTag maybeTag maybeCount offset = do -}
    {-let f = case maybeTag of -}
                {-Nothing -> []-}
                {-Just t -> [ SqlImageTag ==. t]-}
    {-let g = case maybeCount of -}
                {-Nothing -> [ Desc SqlImageCreated , OffsetBy offset ]-}
                {-Just count -> [ Desc SqlImageCreated-}
                              {-, LimitTo count -}
                              {-, OffsetBy offset-}
                              {-]-}
    {-map getPair <$> runDB (selectList f g)-}
 {-where -}
    {-getPair (a,b) = (a, fromSqlImage b) -}
{-getImageByTag :: (YesodPersistBackend master ~ SqlPersist, YesodPersist master ,PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) => Maybe Text-> Maybe Int-> Int-> GHandler sub master [(SqlImageId,Image)]-}
getImageByTag maybeTag maybeCount offset = case maybeTag of 
        Nothing -> map (\(a,b) -> (a , fromSqlImage b) ) <$> runDB (selectList [] (maybe [Desc SqlImageCreated , OffsetBy offset] (\count -> [Desc SqlImageCreated , LimitTo count , OffsetBy offset ]) maybeCount) )
        Just t -> do
            let g = maybe [Desc TagCreated , OffsetBy offset] (\count -> [Desc TagCreated , LimitTo count ,OffsetBy offset]) maybeCount
            listImages <- map getSqlImageIds <$> runDB (selectList [TagTag ==. t] g)
            listMaybepairs <- pairSequence $  map (\a -> (a,getImagePersist a)) listImages
            return $ paircatMaybes listMaybepairs
 where 
    getSqlImageIds :: (a,Tag) -> SqlImageId
    getSqlImageIds (a,b) = tagImageId b 
    pairSequence :: (Monad m,Functor m) => [(a , m b)] -> m [(a,b)]
    pairSequence [] = return []
    pairSequence ((a,b):xs) = do 
        temp <- b
        fmap ((:) (a,temp)) (pairSequence xs) 
    paircatMaybes :: [(a, Maybe b)] -> [(a, b)]
    paircatMaybes [] = [] 
    paircatMaybes ((a,b):xs) = case b of 
        Just image -> (a,image):paircatMaybes xs
        Nothing -> paircatMaybes xs 


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
    , jSrc = T.append sUploadDirectory (name image)
    , jTag = tags image 
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
getVotePersist :: (YesodPersist master,PersistBackend(YesodPersistBackend master) (GGHandler sub master IO)) =>Key backend User-> Key backend SqlImage-> GHandler sub master (Maybe(Key (YesodPersistBackend master) (VotesGeneric backend),VotesGeneric backend))
getVotePersist uid id  = runDB.getBy $ UniqueVote uid id


-- stores the given Vote In the database
storeVotesPersist :: (YesodPersist master,PersistEntity val,PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) =>val -> GHandler sub master ()
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
                    let a = T.pack $ take 20 (randomRs ('a','z') gen) 
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
    mayBeImage <- getImagePersist id  
    return $ fmap owner mayBeImage

--requireAdmin :: Handler ()
requireAdmin ownerid = do
    currentid <- requireAuthId
    case ownerid of 
        Nothing -> permissionDenied "Image not found"
        Just (Nothing) -> permissionDenied "This image is orphan"
        Just (Just x) -> if currentid == x then return () else permissionDenied "You are not authorized to touch this!!"

canIVote :: UserId -> SqlImageId -> Handler Bool
canIVote uid id = do 
    alreadyInVotes <- getVotePersist uid id 
    case alreadyInVotes of
        Nothing -> return True
        Just (qid , val) -> 
            case votesValue val of
                1 -> return False 
                _ -> return True

canDVote :: UserId -> SqlImageId -> Handler Bool
canDVote uid id = do 
    alreadyInVotes <- getVotePersist uid id 
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

