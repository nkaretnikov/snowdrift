{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import TestImport
import Yesod.Default.Config
-- import Yesod.Test
import Test.Hspec (hspec, describe, it)
import Application (makeFoundation)

import UserTest
import NotifyTest
import DiscussionTest
import WikiTest
import BlogTest
import RethreadTest

import TestHandler
import Model.Markdown

import Control.Exception (bracket)
import System.Directory (removeFile, getTemporaryDirectory)
import System.IO
import System.IO.Unsafe

main :: IO ()
main = do
    liftIO $ hPutStrLn stderr "starting test program" >> hFlush stderr
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                { csParseExtra = parseExtra
                }

    liftIO $ hPutStrLn stderr "building foundation" >> hFlush stderr
    foundation <- makeFoundation conf


    liftIO $ hPutStrLn stderr "running test" >> hFlush stderr

    withTempFile $ spec foundation

withTempFile :: (FilePath -> IO a) -> IO ()
withTempFile f = bracket
    (do tmp <- getTemporaryDirectory; openTempFile tmp "emails")
    (removeFile . fst)
    (\ (file, handle) -> do hClose handle; void $ f file)

spec :: App -> FilePath -> IO ()
spec foundation file =
    hspec $ do
        describe "fix links" $ do
            it "works correctly on all examples" $ do
                let mismatches = unsafePerformIO $ testHandler testFixLinks
                case mismatches of
                    Right [] -> True
                    _ -> False

        yesodSpec foundation $ do
            -- userSpecs
            -- notifySpecs (settings foundation) file
            -- wikiSpecs
            -- blogSpecs
            -- discussionSpecs
            rethreadSpecs foundation
            return ()           -- XXX: uncomment back
