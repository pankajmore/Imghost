{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Helpers.Document
    ( getRandomName
    , getName
    , getExtension
    )where
import Foundation
import System.Random
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
                        then return (a ++ ending)
                        else getRandomName ending
checkDatabase :: String -> GHandler ImgHost ImgHost Bool
checkDatabase image = do 
           maybeImage <- runDB (getBy $ ImageName image)
           case maybeImage of
               Just _ -> return True
               _ -> return False

getName :: String ->  String
getName = takeWhile (/='.')

getExtension :: String ->  String
getExtension = dropWhile (/='.')

