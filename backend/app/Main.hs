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
        java.util.List<java.lang.ClassLoader> classLoadersList = new java.util.LinkedList<java.lang.ClassLoader>();
        classLoadersList.add(org.reflections.util.ClasspathHelper.contextClassLoader());
        classLoadersList.add(org.reflections.util.ClasspathHelper.staticClassLoader());


        org.reflections.Reflections reflections = new org.reflections.Reflections(
            new org.reflections.util.ConfigurationBuilder()
                .setScanners(
                    new org.reflections.scanners.SubTypesScanner(false /* don't exclude Object.class */),
                    new org.reflections.scanners.ResourcesScanner()
                )
                .setUrls(org.reflections.util.ClasspathHelper.forClassLoader(classLoadersList.toArray(new java.lang.ClassLoader[0])))
                .filterInputsBy(new org.reflections.util.FilterBuilder().includePackage("org.bridget"))
        );

        java.lang.String s = reflections.getSubTypesOf(java.lang.Object.class).toString();
      
        System.out.println("Subtypes found: " + s); // Added for demonstration of output
      int[][] blockShape = org.bridget.Main.tBlockShape;
      org.bridget.PieceInventory pi = new org.bridget.PieceInventory();
      System.out.println(pi.hasPiece("L"));
      System.out.println(blockShape.toString());
      }
   |]