{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Vote ( postVotedR
                    , postVoteiR
                    , getVoteiR
                    , getVotedR
                    ) where
import Foundation
import Forms.Image
import Helpers.Document
import Helpers.Storage
postVotedR :: ImagesId ->Handler RepHtml
postVotedR id = do
    ((dresult, dwidget), denctype) <- runFormPost (imageForm images_thumbsdown_jpg) 
    case dresult of
        FormSuccess _ -> do
                        currentUserid <- requireAuthId
                        alreadyInVotes <- getVotePersist currentUserid id 
                        case alreadyInVotes of 
                            Nothing -> do 
                                let vote = Votes { votesUserId = currentUserid
                                                           , votesImageId = id
                                                           , votesValue = (-1)
                                                           }
                                storeVotesPersist vote
                                updateById id [SqlImageVotes -=. 1]
                            Just (qid , val) -> do
                                                case votesValue val of
                                                    0 ->  do
                                                        updateById qid [VotesValue =. (-1)]
                                                        updateById id [SqlImageVotes -=. 1]
                                                    1 -> do
                                                        updateById qid [VotesValue =. 0]
                                                        updateById id [SqlImageVotes -=. 1]
                                                    (-1) -> return ()
                        redirect RedirectTemporary $ ImageR id
        _ ->  redirect RedirectTemporary $ ImageR id

postVoteiR :: ImagesId ->Handler RepHtml
postVoteiR id = do
    ((iresult, iwidget), ienctype) <- runFormPost (imageForm images_thumbsup_jpg)
    case iresult of
        FormSuccess _ -> do 
                        currentUserid <- requireAuthId
                        alreadyInVotes <- getVotePersist currentUserid id 
                        case alreadyInVotes of 
                            Nothing -> do 
                                let vote = Votes { votesUserId = currentUserid
                                                           , votesImageId = id
                                                           , votesValue = (-1)
                                                           }
                                storeVotesPersist vote
                                updateById id [SqlImageVotes +=. 1]
                            Just (qid , val) -> do 
                                                case votesValue val of
                                                    0 ->  do
                                                        updateById qid [VotesValue =. 1]
                                                        updateById id [SqlImageVotes +=. 1]
                                                    (-1) -> do
                                                        updateById qid [VotesValue =. 0]
                                                        updateById id [SqlImageVotes +=. 1]
                                                    1 -> return ()
                        redirect RedirectTemporary $ ImageR id 
        _ ->  redirect RedirectTemporary $ ImageR id
getVoteiR = postVoteiR
getVotedR = postVotedR
