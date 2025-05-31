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
import Foreign.JNI (showException, withJVM)
import Language.Java.Inline


main :: IO ()
main = do
    r <- Runfiles.create
    let jarPath = Runfiles.rlocation r "bridget_ui_ws/jar_deploy.jar"
        jvmArgs = [ "-Djava.class.path=" <> fromString jarPath ]
    withJVM jvmArgs $ do
        handle (showException >=> Text.putStrLn) [java| { System.out.println("Hi Haskell!");} |]

        liftIO $ scotty 3000 $ do
            get "/" $ do
                html "<h1>Welcome To bridget ui!</h1>"

            get "/greet/:name" $ do
                name <- param "name" :: ActionM Text
                -- Interact with JVM to process the name
                -- Note: `java` quasi-quote implicitly uses the JVM context from `withJVM`
                -- `liftAndCatchIO` is used to run `IO` actions (like `java`) within Scotty's `ActionM` monad
                javaMessage <- liftAndCatchIO $ do
                    [java| {
                        public String getGreeting(String name) {
                            return "Hello, " + name + " from Java!";
                        }
                    }
                    |]
                    -- Call the Java method, passing the Haskell Text parameter
                    -- `reify` and `reflect` handle conversion between Haskell and Java types
                    [java| @(String) getGreeting($(String name)) |]
                html $ "<h2>" <> javaMessage <> "</h2>"

    -- The `scotty` function runs indefinitely.