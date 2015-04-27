{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs         #-}

module RethreadTest (rethreadSpecs) where

import Import hiding (count, commentParent, exists, rethreadComment, nothing)
import qualified Import
import TestImport hiding (count, app, get, commentParent, rethreadComment)

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import qualified Data.Text as Text

import Model.Comment
import Model.Discussion

testSDB :: App -> SDB a -> Example a
testSDB app =
    liftIO . runNoLoggingT . flip runReaderT app .  runResourceT . runSDB

-- | Check that rethreading works as expected, that is, only the
-- needed comments are rethreaded.  Note that the order is important
-- here.
rethreadSpecs :: App -> Spec
rethreadSpecs app = yit "rethreading" $ testSDB app $ [marked|
    discussion_id <- lift createDiscussionDB
    let user_id = key $ PersistInt64 1
    -- comment1
    -- comment2
    -- comment3
    -- comment4
    -- comment5
    -- comment6
    -- 'RecordWildCards' doesn't work in 'marked'.
    comment_ids <- lift $ insertComments discussion_id user_id
    let comment1 = comment_id1 comment_ids
        comment2 = comment_id2 comment_ids
        comment3 = comment_id3 comment_ids
        comment4 = comment_id4 comment_ids
        comment5 = comment_id5 comment_ids
        comment6 = comment_id6 comment_ids
    -- comment2
    -- comment3
    -- `-comment1
    -- comment4
    -- comment5
    -- comment6
    testRight $ rethread comment1 comment3 discussion_id user_id
    lift $ errorUnlessParentDepthDiscussion comment2 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment3 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment1 (Just comment3) 1 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment4 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment5 Nothing         0 discussion_id
    lift $ errorUnlessCommentRethreaded comment1 Nothing (Just comment3) user_id
    lift $ errorUnlessDifferentRethreadCount 1
    -- comment2
    -- comment4
    -- `-comment3
    --  `-comment1
    -- comment5
    -- comment6
    testRight $ rethread comment3 comment4 discussion_id user_id
    lift $ errorUnlessParentDepthDiscussion comment2 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment4 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment3 (Just comment4) 1 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment1 (Just comment3) 2 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment5 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment6 Nothing         0 discussion_id
    lift $ errorUnlessCommentRethreaded comment3 Nothing (Just comment4) user_id
    lift $ errorUnlessDifferentRethreadCount 3
    -- This is the requested change:
    --
    -- comment2
    -- comment3
    -- `-comment1
    --  `-comment4
    -- comment5
    -- comment6
    --
    -- Instead, an error is returned and no actions (besides lookups)
    -- are performed:
    --
    -- comment2
    -- comment4
    -- `-comment3
    --  `-comment1
    -- comment5
    -- comment6
    testLeft $ rethread comment4 comment1 discussion_id user_id
    lift $ errorUnlessParentDepthDiscussion comment2 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment4 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment3 (Just comment4) 1 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment1 (Just comment3) 2 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment5 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment6 Nothing         0 discussion_id
    -- 'errorUnlessCommentRethreaded' omitted on purpose.
    lift $ errorUnlessDifferentRethreadCount 3
    -- comment1
    -- comment2
    -- comment4
    -- `-comment3
    -- comment5
    -- comment6
    testRight $ rethreadToTopLevel comment1 discussion_id user_id
    lift $ errorUnlessParentDepthDiscussion comment1 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment2 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment4 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment3 (Just comment4) 1 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment5 Nothing         0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment6 Nothing         0 discussion_id
    lift $ errorUnlessCommentRethreaded comment1 (Just comment3) Nothing user_id
    lift $ errorUnlessDifferentRethreadCount 4
    -- comment1
    -- comment2
    -- comment3
    -- comment4
    -- comment5
    -- comment6
    testRight $ rethreadToTopLevel comment3 discussion_id user_id
    lift $ errorUnlessParentDepthDiscussion comment1 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment2 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment3 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment4 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment5 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment6 Nothing 0 discussion_id
    lift $ errorUnlessCommentRethreaded comment3 (Just comment4) Nothing user_id
    lift $ errorUnlessDifferentRethreadCount 5
    -- Test that a discussion id is being changed on rethreading.
    discussion_id' <- lift createDiscussionDB
    testRight $ rethreadToTopLevel comment2 discussion_id' user_id
    lift $ errorUnlessParentDepthDiscussion comment1 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment2 Nothing 0 discussion_id'
    lift $ errorUnlessParentDepthDiscussion comment3 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment4 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment5 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment6 Nothing 0 discussion_id
    lift $ errorUnlessCommentRethreaded comment2 Nothing Nothing user_id
    lift $ errorUnlessDifferentRethreadCount 6
    -- Test that a user id is being changed on rethreading.
    let user_id' = key $ PersistInt64 2
    testRight $ rethreadToTopLevel comment4 discussion_id user_id'
    lift $ errorUnlessParentDepthDiscussion comment1 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment2 Nothing 0 discussion_id'
    lift $ errorUnlessParentDepthDiscussion comment3 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment4 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment5 Nothing 0 discussion_id
    lift $ errorUnlessParentDepthDiscussion comment6 Nothing 0 discussion_id
    lift $ errorUnlessCommentRethreaded comment4 Nothing Nothing user_id'
    lift $ errorUnlessDifferentRethreadCount 7
    |]

-- | Since it's easier not to be tied to the actual data in the DB and just
-- index comments starting from 1, the 'CommentIds' record stores the
-- actual comment ids.
data CommentIds = CommentIds
    { comment_id1 :: CommentId, comment_id2 :: CommentId, comment_id3 :: CommentId
    , comment_id4 :: CommentId, comment_id5 :: CommentId, comment_id6 :: CommentId }

-- | Insert comments to the same discussion, without parents or children.
insertComments :: DiscussionId -> UserId -> DB CommentIds
insertComments discussion_id user_id = do
    let ins text = insertComment discussion_id Nothing user_id text 0
    c1 <- ins "comment 1"
    c2 <- ins "comment 2"
    c3 <- ins "comment 3"
    c4 <- ins "comment 4"
    c5 <- ins "comment 5"
    c6 <- ins "comment 6"
    return $ CommentIds { comment_id1 = c1, comment_id2 = c2, comment_id3 = c3
                        , comment_id4 = c4, comment_id5 = c5, comment_id6 = c6 }

-- XXX: Move to 'Model' and use it in 'postRethreadComment' in
-- 'Handler.Comment'.
depthOffset :: Maybe CommentId -> Maybe CommentId -> DB Int
depthOffset mold_parent_id mnew_parent_id = do
    let maybeDepth mcid = case mcid of
            Nothing  -> return Nothing
            Just cid -> get cid >>= return . fmap commentDepth
    mold_parent_depth <- maybeDepth mold_parent_id
    mnew_parent_depth <- maybeDepth mnew_parent_id
    return $
        (fromMaybe (-1) mold_parent_depth) -
        (fromMaybe (-1) mnew_parent_depth)

commentParent :: CommentId -> DB (Maybe CommentId)
commentParent comment_id =
    get comment_id >>= \mcomment ->
        return $ join $ fmap Import.commentParent mcomment

-- | Test that is expected to succeed.
testRight :: SDB (Either Text ()) -> SDB ()
testRight res = res >>= \case
    Left err -> error $ Text.unpack err
    Right v  -> return v

-- | Test that is expected to fail.
testLeft :: SDB (Either Text ()) -> SDB ()
testLeft res = res >>= \case
    Left err -> liftIO $ print $ Text.unpack err
    Right _  -> error "expected a 'Left', but got a 'Right'"

-- | Use 'rethread' or 'rethreadToTopLevel' instead, which make passing
-- incorrect arguments more difficult.
rethreadComment :: CommentId -> Maybe CommentId -> DiscussionId -> UserId
                -> SDB (Either Text ())
rethreadComment comment_id mnew_parent_id discussion_id user_id = [marked|
    mold_parent_id <- lift $ commentParent comment_id
    depth_offset   <- lift $ depthOffset mold_parent_id mnew_parent_id
    rethreadCommentDB mnew_parent_id discussion_id comment_id
        user_id "testing" depth_offset
    |]

rethread :: CommentId -> CommentId -> DiscussionId -> UserId
         -> SDB (Either Text ())
rethread comment_id new_parent_id discussion_id user_id =
    rethreadComment comment_id (Just new_parent_id) discussion_id user_id

rethreadToTopLevel :: CommentId -> DiscussionId -> UserId
                   -> SDB (Either Text ())
rethreadToTopLevel comment_id discussion_id user_id =
    rethreadComment comment_id Nothing discussion_id user_id

-- | Insert a comment.  A comment without a parent must have depth 0.
-- Otherwise, the comment's depth is a successor of the parent's
-- depth.
insertComment :: DiscussionId -> Maybe CommentId -> UserId -> Markdown
              -> Int -> DB CommentId
insertComment discussion_id mparent_id user_id text depth = [marked|
    now <- liftIO getCurrentTime
    insert $ Comment now (Just now) (Just user_id) discussion_id mparent_id
                     user_id text depth VisPublic defaultLanguage
    |]

selectExistsRethread :: CommentId -> Maybe CommentId -> Maybe CommentId
                     -> UserId -> DB Bool
selectExistsRethread comment_id mold_parent mnew_parent user_id = [marked|
    selectExists $ from $ \r -> do
        where_ $ r ^. RethreadComment   ==.               val comment_id
             &&. r ^. RethreadOldParent `notDistinctFrom` val mold_parent
             &&. r ^. RethreadNewParent `notDistinctFrom` val mnew_parent
             &&. r ^. RethreadModerator ==.               val user_id
    |]

class Show a => PPrint a where
    pprint :: a -> String

instance PPrint CommentId where
    pprint :: CommentId -> String
    pprint = show . unSqlBackendKey . unCommentKey

instance PPrint (Maybe CommentId) where
    pprint :: Maybe CommentId -> String
    pprint (Just k) = pprint k
    pprint nothing  = show nothing

instance PPrint DiscussionId where
    pprint :: DiscussionId -> String
    pprint = show . unSqlBackendKey . unDiscussionKey

instance PPrint Int where
    pprint :: Int -> String
    pprint = show

-- | Check that a comment was rethreaded.
errorUnlessCommentRethreaded :: CommentId -> Maybe CommentId -> Maybe CommentId
                             -> UserId -> DB ()
errorUnlessCommentRethreaded comment_id mold_parent mnew_parent user_id = [marked|
    exists <- selectExistsRethread comment_id mold_parent mnew_parent user_id
    unless exists $ error $
        "failed to rethread comment " <> pprint comment_id <>
        (maybe "" ((" from parent " <>) . pprint) mold_parent) <>
        (maybe "" ((" to parent "   <>) . pprint) mnew_parent)
    |]

-- | This allows to check that other comments were not rethreaded as a
-- side-effect of rethreading some comment.
errorUnlessDifferentRethreadCount :: Int -> DB ()
errorUnlessDifferentRethreadCount count = [marked|
    count' <- selectCount $ from $ \r -> do
         -- A hint for the typechecker that doesn't require
         -- 'ScopedTypeVariables', which is not supported by 'marked'.
        let _ = r :: SqlExpr (Entity Rethread)
        return ()
    unless (count == count') $ error $
        "expected rethread count " <> pprint count <>
        ", but got " <> pprint count'
    |]

-- | Ensure that a comment has the right parent, depth, and discussion
-- id.  These fields may change when the comment is rethreaded.
errorUnlessParentDepthDiscussion :: CommentId -> Maybe CommentId -> Int
                                 -> DiscussionId -> DB ()
errorUnlessParentDepthDiscussion comment_id mparent_id depth discussion_id = [marked|
    exists <- selectExists $ from $ \c -> do
        where_ $ c ^. CommentId         ==.               val comment_id
             &&. c ^. CommentParent     `notDistinctFrom` val mparent_id
             &&. c ^. CommentDepth      ==.               val depth
             &&. c ^. CommentDiscussion ==.               val discussion_id
    unless exists $ error $
        "failed to find comment " <> pprint comment_id <>
        " with parent "           <> pprint mparent_id <>
        ", depth "                <> pprint depth      <>
        ", discussion "           <> pprint discussion_id
    |]
