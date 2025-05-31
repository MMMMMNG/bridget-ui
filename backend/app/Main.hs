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
    withJVM jvmArgs $ handle (showException >=> Text.putStrLn) [java| {
      org.reflections.Reflections reflections = new org.reflections.Reflections(".");

      String s = reflections.getSubTypesOf(Object.class).toString();
      //String s = Main.tBlockShape.toString();
      System.out.println(s);
      }
   |]