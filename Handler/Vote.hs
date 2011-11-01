{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Vote (postVotedR,postVoteiR) where
import Foundation
import Forms.Image
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import Control.Applicative
import qualified Data.Text as T
postVotedR :: ImagesId ->Handler RepHtml
postVotedR id = do
    ((dresult, dwidget), denctype) <- runFormPost (imageForm "/static/images/thumbsdown.jpg") 
    case dresult of
        FormSuccess _ -> do 
                        runDB (update id [ImagesVotes -=. 1])
                        redirect RedirectTemporary $ ImageR id
        _ ->  redirect RedirectTemporary $ ImageR id

postVoteiR :: ImagesId ->Handler RepHtml
postVoteiR id = do
    ((iresult, iwidget), ienctype) <- runFormPost (imageForm "/static/images/thumbsup.jpg")
    case iresult of
        FormSuccess _ -> do 
                        runDB (update id [ImagesVotes +=. 1])
                        redirect RedirectTemporary $ ImageR id 
        _ ->  redirect RedirectTemporary $ ImageR id
    

