{-# LANGUAGE OverloadedStrings #-}

module Test.Documents where

import Test.Common
import Test.Import

spec :: Spec
spec =
  describe "document API" $ do
    it "indexes, updates, gets, and then deletes the generated document" $ withTestEnv $ do
      _ <- insertData
      _ <- updateData
      docInserted <- getDocument testIndex testMapping (DocId "1")
      let newTweet = eitherDecode
                     (responseBody docInserted) :: Either String (EsResult Tweet)
      liftIO $ fmap getSource newTweet `shouldBe` Right (Just patchedTweet)

    it "indexes, gets, and then deletes the generated document with a DocId containing a space" $ withTestEnv $ do
      _ <- insertWithSpaceInId
      docInserted <- getDocument testIndex testMapping (DocId "Hello World")
      let newTweet = eitherDecode
                     (responseBody docInserted) :: Either String (EsResult Tweet)
      liftIO $ fmap getSource newTweet `shouldBe` Right (Just exampleTweet)

    it "produces a parseable result when looking up a bogus document" $ withTestEnv $ do
      doc <- getDocument testIndex testMapping  (DocId "bogus")
      let noTweet = eitherDecode
                    (responseBody doc) :: Either String (EsResult Tweet)
      liftIO $ fmap foundResult noTweet `shouldBe` Right Nothing

    it "can use optimistic concurrency control" $ withTestEnv $ do
      let ev = ExternalDocVersion minBound
      let cfg = defaultIndexDocumentSettings { idsVersionControl = ExternalGT ev }
      resetIndex
      res <- insertData' cfg
      liftIO $ isCreated res `shouldBe` True
      res' <- insertData' cfg
      liftIO $ isVersionConflict res' `shouldBe` True

    it "indexes two documents in a parent/child relationship and checks that the child exists" $ withTestEnv $ do
      resetIndex
      _ <- putMapping testIndex (MappingName "child") ChildMapping
      _ <- putMapping testIndex (MappingName "parent") ParentMapping
      _ <- indexDocument testIndex (MappingName "parent") defaultIndexDocumentSettings exampleTweet (DocId "1")
      let parent = (Just . DocumentParent . DocId) "1"
          ids = IndexDocumentSettings NoVersionControl parent
      _ <- indexDocument testIndex (MappingName "child") ids otherTweet (DocId "2")
      _ <- refreshIndex testIndex
      exists <- documentExists testIndex (MappingName "child") parent (DocId "2")
      liftIO $ exists `shouldBe` True

    it "updates documents by query" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      _ <- insertExtra
      let query = (TermQuery (Term "user" "bitemyapp") Nothing)
          script =
            Script
            (Just (ScriptLanguage "painless"))
            (Just (ScriptInline "ctx._source.age *= 2"))
            Nothing
            Nothing
      _ <- updateByQuery testIndex query (Just script)
      _ <- refreshIndex testIndex
      let search = mkSearch (Just query) Nothing
      parsed <- searchTweets search
      liftIO $ print parsed
      case parsed of
        Left e ->
          liftIO $ expectationFailure ("Expected tweets as search result but got: " <> show e)
        Right sr -> do
          let results =
                map (fmap age . hitSource) (hits (searchHits sr))
          liftIO $
            results `shouldBe` [Just $ age exampleTweet * 2, Just $ age tweetWithExtra * 2]
