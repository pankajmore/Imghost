{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Image (getImageR,postImageR) where
import Foundation
import Forms.Upload
import Data.Time
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Yesod.Comments
import Forms.Vote
import Yesod.Goodies
getImageR :: ImagesId -> Handler RepHtml
getImageR id = do
                ((iresult, iwidget), ienctype) <- generateFormPost voteIForm
                ((dresult, dwidget), denctype) <- generateFormPost voteDForm
                im <- getImage id
                case im of
                    Just (iName,tag,votes,cTime) ->do 
                                    let image = sUploadDirectory ++ (iName)
                                    let image2 = getThumb image
                                    createdTime <- humanReadableTime cTime
                                    defaultLayout $ do
                                        urlbox <- lift newIdent
                                        tableProperty <- lift newIdent
                                        $(widgetFile "image")
                                        addComments $ T.pack iName
                    Nothing -> do setMessage "ID not found in the database"
                                  redirect RedirectTemporary RootR

postImageR = getImageR

