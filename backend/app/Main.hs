{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Exception (handle)
import Data.String (fromString)
import Control.Monad ((>=>))
import qualified Bazel.Runfiles as Runfiles
import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import Foreign.JNI (showException, withJVM, runInAttachedThread)
import Language.Java.Inline
import Language.Java (reflect)
import Language.Java (reify)

import Web.Scotty
import qualified Data.Text.Lazy as TL

import Control.Concurrent (runInBoundThread)


putStringThroughJVM :: String -> IO DT.Text
putStringThroughJVM thestr = runInBoundThread $ runInAttachedThread $ do
    rn <- reflect (DT.pack (thestr <> " woah dude"))
    s <- [java| {
                String strr = $rn;
                return String.join(", ", strr, "yay"); 
            }
        |]
    ss <- reify s
    pure (ss :: DT.Text)

scottyPutStringJVM :: String -> ActionM DT.Text
scottyPutStringJVM a = liftIO $ putStringThroughJVM a





main :: IO ()
main = do
    r <- Runfiles.create
    let jarPath = Runfiles.rlocation r "bridget_ui_ws/jar_deploy.jar"
        jvmArgs = [ "-Djava.class.path=" <> fromString jarPath ]
    withJVM jvmArgs $ do
        st <- reflect (DT.pack "HENLO")
        handle (showException >=> DTIO.putStrLn) [java| { 
                    System.out.println($st);
                } 
            |]

        rr <- putStringThroughJVM "NAOAOAO"
        DTIO.putStrLn rr

        liftIO $ scotty 3000 $ do
            get "/" $ do
                html "<h1>Welcome To bridget ui!</h1>"

            get "/greet/:name" $ do
                name <- pathParam "name"
                liftIO $ putStrLn (name :: String)


                javaMessage <- scottyPutStringJVM "egz" 

                html $ "<h2>" <> TL.fromStrict javaMessage <> TL.pack name <> "</h2>"

    -- The `scotty` function runs indefinitely.