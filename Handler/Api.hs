{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Api (postApiR) where
import Foundation
import Forms.Upload
import Helpers.Document
import Helpers.Storage
import qualified Data.Text as T
import System.Cmd (system)
import Data.Time (getCurrentTime)
import qualified Data.ByteString.Lazy as L
postApiR :: Handler ()
postApiR = do
    content <- runRequestBody
    let maybeTag = lookup "tag"$ fst content 
    case maybeTag of 
        Just t -> if (t,t) `elem` defaultTags 
            then do 
                let fileInfo = snd.head.snd $ content 
                let extension = getExtension $ fileName fileInfo
                if T.isPrefixOf "image" (fileContentType fileInfo) 
                    then do 
                        randName <- getRandomName extension
                        time <- liftIO getCurrentTime
                        mid <- maybeAuthId
                        let image = Image { name = randName
                                          , tag = t  
                                          , owner = mid 
                                          , caption = getName $ fileName fileInfo
                                          , votes = 0 
                                          , created = time
                                          }
                        liftIO $ L.writeFile (T.unpack $ T.append uploadDirectory randName) $ fileContent fileInfo
                        liftIO $ system . T.unpack  $ T.concat ["convert ",uploadDirectory,randName," -thumbnail 100x100^ -gravity center -extent 100x100 ",uploadDirectory,getThumb randName]
                        id <- storeImagePersist image
                        return ()
                    else liftIO $ print ("Not an image" ++ T.unpack (fileContentType fileInfo))
            else liftIO $ print "Not a default tag given"
        _ -> liftIO $ print "tag not given"
    return ()


