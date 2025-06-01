{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QualifiedDo #-}
module Main where

import Control.Exception (handle)
import Data.String (fromString)
import Control.Monad ((>=>))
import qualified Bazel.Runfiles as Runfiles
import qualified Data.Text as DT
import qualified Data.Text.IO as DTIO
import Foreign.JNI (showException, withJVM, runInAttachedThread)
import qualified Language.Java as NonLinear
import Language.Java.Inline.Safe
import Language.Java.Safe

import Web.Scotty
import qualified Data.Text.Lazy as TL

import Control.Concurrent (runInBoundThread)
import Prelude.Linear (Ur(..))
import qualified System.IO.Linear as Linear
import qualified Control.Functor.Linear as Linear
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
readGameState jGameState = Linear.do
  -- Read boolean fields
  jGameOver <- [java| $jGameState.isGameOver() |]
  gameOverHaskell <- NonLinear.reify jGameOver

  jMoveInvalid <- [java| $jGameState.isMoveInvalid() |]
  moveInvalidHaskell <- NonLinear.reify jMoveInvalid

  -- Read String winner
  jWinner <- [java| $jGameState.getWinner() |]
  winnerHaskell <- NonLinear.reify jWinner

  -- Read int[][][] board
  jBoard <- [java| $jGameState.getBoard() |]
  -- Reify converts a Java array to a Haskell list of lists
  boardHaskell <- NonLinear.reify jBoard

  Linear.return $ Ur GameState
    { gameOver = gameOverHaskell
    , moveInvalid = moveInvalidHaskell
    , winner = winnerHaskell
    , board = boardHaskell
    }


makeMove :: DT.Text -> DT.Text -> ActionM GameState
makeMove algo mv = liftIO $ runInBoundThread $ runInAttachedThread $ do
    jal <- NonLinear.reflect algo
    jmv <- NonLinear.reflect mv

    jgs <- [java| brgt.GameState.playerMove($jal, $jmv) |]
    gs <- NonLinear.reify jgs
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

                html $ "<h2>" <> TL.fromStrict javaMessage <> TL.pack name <> "</h2>"

    -- The `scotty` function runs indefinitely.