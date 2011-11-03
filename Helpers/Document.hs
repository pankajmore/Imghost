{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Helpers.Document
    ( getRandomName
    , getName
    , getExtension
    , getImage
    , getOwner
    , getThumb
    , deleteImage
    , staticUpload
    , requireAdmin
    , canIVote
    , canDVote
    , canDeleteImage
    )where
import Foundation
import System.Random
import Control.Applicative 
import Directory (removeFile)
import qualified Data.Text as T
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
           maybeImage <- runDB (getBy $ UniqueName image)
           case maybeImage of
               Just _ -> return True
               _ -> return False

getName :: Text ->  Text
getName = T.takeWhile (/='.')

getThumb :: Text -> Text
getThumb iName = T.replace "." "-thumb." iName


getExtension :: Text ->  Text
getExtension = T.dropWhile (/='.')

{-getImage id = fmap (\x -> (imagesImageName x,imagesImageTag x,imagesCaption x,imagesVotes x,imagesCreated x)) <$> runDB (get id)-}

getOwner id = fmap imagesOwner <$> runDB (get id)

--requireAdmin :: Handler ()
requireAdmin ownerid = do
    currentid <- requireAuthId
    case ownerid of 
        Nothing -> permissionDenied "Image not found"
        Just (Nothing) -> permissionDenied "This image is orphan"
        Just (Just x) -> if currentid == x then return () else permissionDenied "You are not authorized to touch this!!"
      --  _ -> permissionDenied "Random reason"
staticUpload :: Text -> StaticRoute 
staticUpload x = StaticRoute ["upload",x] [("","")]   

canIVote :: UserId -> ImagesId -> Handler Bool
canIVote uid id = do 
    alreadyInVotes <- runDB (getBy $ UniqueVote uid id)
    case alreadyInVotes of
        Nothing -> return True
        Just (qid , val) -> 
            case votesValue val of
                1 -> return False 
                _ -> return True

canDVote :: UserId -> ImagesId -> Handler Bool
canDVote uid id = do 
    alreadyInVotes <- runDB (getBy $ UniqueVote uid id)
    case alreadyInVotes of
        Nothing -> return True
        Just (qid , val) -> 
            case votesValue val of
                (-1) -> return False 
                _ -> return True

canDeleteImage :: UserId -> ImagesId -> Handler Bool
canDeleteImage uid id = do
            ownerid <- getOwner id
            case ownerid of
                Nothing -> return False
                Just Nothing -> return False
                Just (Just oid) -> if uid == oid then return True else return False
