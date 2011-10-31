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
getImageR :: ImagesId -> Handler RepHtml
getImageR id = do
                ((iresult, dwidget), ienctype) <- generateFormPost voteIForm
                ((dresult, iwidget), denctype) <- generateFormPost voteDForm
                im <- getImage id
                case im of
                    Just (iName,tag,votes,createdTime) ->do 
                                    let image = sUploadDirectory ++ (iName)
                                    let image2 = getThumb image
                                    defaultLayout $ do
                                        urlbox <- lift newIdent
                                        $(widgetFile "image")
                                        addComments $ T.pack iName
                    Nothing -> do setMessage "ID not found in the database"
                                  redirect RedirectTemporary RootR

postImageR = getImageR

