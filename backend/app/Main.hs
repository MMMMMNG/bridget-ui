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
import Foreign.JNI.Types
import Language.Java.Inline
import Language.Java (reify, reflect, Interpretation, Reify, Reflect)

import Web.Scotty
import qualified Data.Text.Lazy as TL

import Control.Concurrent (runInBoundThread)

data GameState = GameState
  { gameOver    :: Bool
  , moveInvalid :: Bool
  , winner      :: DT.Text
  , board       :: [[[Int]]]
  } deriving (Show, Eq)

withStatic [d|
    instance Interpretation GameState where
        type Interp GameState = 'Class "brgt.GameInterface.GameState"
    
    instance Reify GameState where
        reify jobj = do
            let method = unsafeDupablePerformIO $ do
                  klass <- getClass (SClass "brgt.GameInterface.GameState")
                  m <- getMethodID klass "isGameOver"
                         (methodSignature [] (SPrim "boolean"))
                  deleteLocalRef klass
                  return m
            go <- callBooleanMethod jobj method []
            pure $ GameState {gameOver = go, moveInvalid = False, winner = DT.empty, board = [[[1]]]}
    
    instance Reflect GameState where
        reflect = new
    |]

makeMove :: DT.Text -> DT.Text -> ActionM GameState
makeMove algo mv = liftIO $ runInBoundThread $ runInAttachedThread $ do
    jal <- reflect algo
    jmv <- reflect mv

    jgs <- [java| brgt.GameState.playerMove($jal, $jmv) |]
    gs <- reify jgs
    pure gs

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