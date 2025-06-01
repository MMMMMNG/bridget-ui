{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
module Main where

import Control.Exception (handle)
import Data.String (fromString)
import Control.Monad ((>=>))
import qualified Bazel.Runfiles as Runfiles
import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import Foreign.JNI (showException, withJVM, runInAttachedThread)
--import qualified Language.Java as NonLinear
import Language.Java
import Language.Java.Inline.Safe
--import Language.Java.Safe
import Foreign.JNI.Safe (newLocalRef)

import Web.Scotty
import qualified Data.Text.Lazy as TL

import Control.Concurrent (runInBoundThread)
import Prelude.Linear (Ur(..))
--import qualified System.IO.Linear as Linear
--import qualified Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear (MonadIO)

import Data.Aeson (ToJSON(..), encode, object, (.=))

data GameState = GameState
  { gameOver    :: Bool
  , moveInvalid :: Bool
  , winner      :: DT.Text
  , board       :: [[[Int]]]
  } deriving (Show, Eq)

-- | Reads a Java GameState object and converts it to a Haskell GameState record.
readGameState :: MonadIO m => J ('Class "brgt.GameInterface$GameState") -> m GameState
readGameState jGameState = liftIO $ do
  -- No newLocalRef needed as we are not in a linear context
  -- Direct Java calls are fine here with the non-linear interface
  jGameOver <- coerce [java| $jGameState.isGameOver() |]
  --gameOverHaskell <- coerce (jGameOver :: J ('Prim "boolean"))

  jMoveInvalid <- coerce [java| $jGameState.isMoveInvalid() |]
  --moveInvalidHaskell <- coerce (jMoveInvalid :: J ('Prim "boolean"))

  jWinner <- [java| $jGameState.getWinner() |]
  winnerHaskell <- reify (jWinner :: J ('Class "java.lang.String") )

  jBoard <- [java| $jGameState.getBoard() |]
  boardHaskell <- reify (jBoard :: J ('Array ('Array ('Array ('Prim "int")))))

  pure GameState
    { gameOver    = gameOverHaskell
    , moveInvalid = moveInvalidHaskell
    , winner      = winnerHaskell
    , board       = boardHaskell
    }


makeMove :: DT.Text -> DT.Text -> ActionM GameState
makeMove algo mv = liftIO $ runInBoundThread $ runInAttachedThread $ do
    jal <- reflect algo
    jmv <- reflect mv

    jgs <- [java| brgt.GameState.playerMove($jal, $jmv) |]
    gs <- reify jgs
    pure gs

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


        liftIO $ scotty 3000 $ do
            get "/" $ do
                html "<h1>Welcome To bridget ui!</h1>"

            post "/move/:mvstr" $ do
                mvstr <- pathParam "mvstr"
                liftIO $ putStrLn ("command: " <> mvstr)

                gs <- makeMove (DT.pack "MINIMAX") (DT.pack mvstr)

                json gs

    -- The `scotty` function runs indefinitely.