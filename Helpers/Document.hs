{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Helpers.Document
    ( getRandomName
    , getName
    , getExtension
    , getImage
    , getThumb
    , deleteImage
    )where
import Foundation
import System.Random
import Control.Applicative 
import Directory (removeFile)
{-
 - Generates a random name of length 20 containing chars from a-z, checks if
 - the database already contains that name, if calls itself again to do another
 - generation and check until it finds a unique name.
-}
getRandomName :: String -> GHandler ImgHost ImgHost String
getRandomName ending = do  
                    gen <- liftIO getStdGen  
                    let a = take 20 (randomRs ('a','z') gen) 
                    liftIO $ newStdGen
                    let isInDatabase = False
                    isInDatabase <- checkDatabase (a ++ ending)
                    if (not isInDatabase)
                        then return (a) --only need the filename not the extension name
                        else getRandomName ending
checkDatabase :: String -> GHandler ImgHost ImgHost Bool
checkDatabase image = do 
           maybeImage <- runDB (getBy $ ImageName image)
           case maybeImage of
               Just _ -> return True
               _ -> return False

getName :: String ->  String
getName = takeWhile (/='.')

getThumb :: String -> String
getThumb iName = getName iName ++ "-thumb" ++ getExtension iName


getExtension :: String ->  String
getExtension = dropWhile (/='.')

getImage id = fmap (\x -> (imagesImageName x,imagesImageTag x,imagesCaption x,imagesVotes x,imagesCreated x)) <$> runDB (get id)

deleteImage id = do
    iName <- fmap imagesImageName <$> runDB (get id)
    case iName of
        Just name -> do 
            let thumbnail = getThumb name 
            liftIO $ removeFile (uploadDirectory ++ thumbnail)
            liftIO $ removeFile (uploadDirectory ++ name)
            runDB (delete id)
        _ -> setMessage "Image not Found"

