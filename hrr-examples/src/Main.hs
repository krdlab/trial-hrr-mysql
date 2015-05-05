module Main where

import Database.HDBC (runRaw, quickQuery', fromSql)
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.Relational.Query
    ( query, relation, relation', placeholder, aggregateRelation
    , on, left, on', fst', snd', (?!), just
    , wheres, (.=.), (.>=.), (!), (><), value, count, groupBy, asc, desc, id'
    , relationalQuery, Relation
    )

import Data.Int (Int32, Int64)
import Data.List (isInfixOf)
import Data.Time (Day, fromGregorian)

import DataSource (connect)
import User (user, User)
import qualified User
import Blog (blog, Blog)
import qualified Blog
import BlogEntry (blogEntry, BlogEntry)
import qualified BlogEntry
import Tag (tag, Tag)
import qualified Tag
import Tagging (tagging, Tagging)
import qualified Tagging
import Comment (comment, Comment)
import qualified Comment

main :: IO ()
main = handleSqlError' $ withConnectionIO connect $ \conn -> do
    printQueryAndResult  conn users
    printQueryAndResult  conn userAndBlog
    printQueryAndResult  conn entryCount
    printQueryAndResult' conn entriesOf 1
    printQueryAndResult  conn blogAndTags
    printQueryAndResult' conn commentsOf 10101
  where
    users :: Relation () User
    users = relation $ query user

    userAndBlog :: Relation () (Int32, Int32)
    userAndBlog = relation $ do
        b <- query blog
        asc $ b ! Blog.userId'
        return $ b ! Blog.userId' >< b ! Blog.id'

    entryCount :: Relation () (Int32, Int64)
    entryCount = aggregateRelation $ do
        be <- query $ blog `left` blogEntry `on'` [\b e -> just (b ! Blog.id') .=. e ?! BlogEntry.blogId']
        let b = be ! fst'
            e = be ! snd'
        g <- groupBy $ b ! Blog.id'
        asc $ g ! id'
        return $ g >< count (just (e ?! BlogEntry.id'))

    entriesOf :: Relation Int32 BlogEntry
    entriesOf = relation' . placeholder $ \uid -> do
        b <- query blog
        e <- query blogEntry
        on $ b ! Blog.id' .=. e ! BlogEntry.blogId'
        wheres $ b ! Blog.userId' .=. uid
        return e

    entryAndTags :: Relation () (Int32, String)
    entryAndTags = relation $ do
        t' <- query tagging
        t  <- query tag
        on $ t' ! Tagging.tagId' .=. t ! Tag.id'
        asc $ t' ! Tagging.blogEntryId' >< t' ! Tagging.tagId'
        return $ t' ! Tagging.blogEntryId' >< t ! Tag.value'

    blogAndTags :: Relation () (Int32, String)
    blogAndTags = relation $ do
        e <- query blogEntry
        t <- query entryAndTags
        on $ e ! BlogEntry.id' .=. t ! fst'
        asc $ e ! BlogEntry.blogId'
        return $ e ! BlogEntry.blogId' >< t ! snd'

    commentsOf :: Relation Int32 Comment
    commentsOf = relation' . placeholder $ \eid -> do
        c <- query comment
        wheres $ c ! Comment.blogEntryId' .=. eid
        asc $ c ! Comment.createdAt'
        return c

    printQueryAndResult  c q    = printQueryAndResult' c q ()
    printQueryAndResult' c q ph = do
        let rq = relationalQuery q
        res <- runQuery' c rq ph
        putStrLn "Query:"
        putStrLn $ "  " ++ show rq
        putStrLn "Result:"
        putStrLn $ "  " ++ show res

