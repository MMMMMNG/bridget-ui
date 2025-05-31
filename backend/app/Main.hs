{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Exception (handle)
import Data.String (fromString)
import Control.Monad ((>=>))
import qualified Bazel.Runfiles as Runfiles
import qualified Data.Text.IO as Text
import Foreign.JNI (showException, withJVM, runInAttachedThread)
import Language.Java.Inline
--import Language.Java (reflect)
import Language.Java (reify)

import Web.Scotty
import qualified Data.Text.Lazy as TL

import Control.Concurrent (runInBoundThread)


main :: IO ()
main = do
    r <- Runfiles.create
    let jarPath = Runfiles.rlocation r "bridget_ui_ws/jar_deploy.jar"
        jvmArgs = [ "-Djava.class.path=" <> fromString jarPath ]
    withJVM jvmArgs $ do
        handle (showException >=> Text.putStrLn) [java| { 
                    System.out.println("Hi Haskell!");
                } 
            |]

        liftIO $ scotty 3000 $ do
            get "/" $ do
                html "<h1>Welcome To bridget ui!</h1>"

            get "/greet/:name" $ do
                name <- pathParam "name" :: ActionM String
                --let rn = reflect name
                -- Interact with JVM to process the name
                -- Note: `java` quasi-quote implicitly uses the JVM context from `withJVM`
                -- `liftAndCatchIO` is used to run `IO` actions (like `java`) within Scotty's `ActionM` monad
                javaMessage <- liftIO $ (runInBoundThread . runInAttachedThread) (do
                    s <- [java| 0.1 + 0.2 |]
                    ss <- reify s
                    pure (ss :: Double)
                    )

                html $ "<h2>" <> TL.pack (show javaMessage) <> TL.pack name <> "</h2>"

    -- The `scotty` function runs indefinitely.