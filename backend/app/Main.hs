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
import Foreign.JNI (showException, runInAttachedThread)
import Language.Java
import Language.Java.Inline
import Web.Scotty
import Network.Wai.Middleware.Cors
import Control.Concurrent (runInBoundThread)
import Data.Aeson (ToJSON(..), object, (.=))

-- | represent the return type from java when making a move
data GameState = GameState
  { gameOver    :: Bool
  , moveInvalid :: Bool
  , winner      :: DT.Text
  , board       :: DT.Text --right, why bother parsing it when it has to be serialized anyway? 
  } deriving (Show, Eq)    -- ...putting the "lazy" back in "lazy string" *ba dum ts* (and yes, I know it's strict)

-- | Reads a Java GameState object and converts it to a Haskell GameState record.
readGameState :: J ('Class "brgt.GameInterface$GameState") -> IO GameState
readGameState jGameState = do
  -- No newLocalRef needed as we are not in a linear context
  -- Direct Java calls are fine here with the non-linear interface
  gameOverHaskell <- [java| $jGameState.isGameOver() |]
  --gameOverHaskell <- coerce (jGameOver :: J ('Prim "boolean"))

  moveInvalidHaskell <- [java| $jGameState.isMoveInvalid() |]
  --moveInvalidHaskell <- coerce (jMoveInvalid :: J ('Prim "boolean"))

  jWinner <- [java| $jGameState.getWinner() |]
  winnerHaskell <- reify (jWinner :: J ('Class "java.lang.String") )

  jBoard <- [java| $jGameState.uglyWorkAroundBoardGet() |]
  boardHaskell <- reify (jBoard :: J ('Class "java.lang.String") )

  pure GameState
    { gameOver    = gameOverHaskell
    , moveInvalid = moveInvalidHaskell
    , winner      = winnerHaskell
    , board       = boardHaskell
    }

-- | Makes GameState serializable (JSON)
instance ToJSON GameState where
  toJSON gs = object [  "isGameOver" .= gameOver gs, 
                        "isLastMoveInvalid" .= moveInvalid gs, 
                        "winner" .= winner gs, 
                        "board" .= board gs]

-- | Attaches to the JVM and calls GameInterface.playerMove(), yielding the return value
makeMove :: DT.Text -> DT.Text -> ActionM GameState
makeMove algo mv = liftIO $ runInBoundThread $ runInAttachedThread $ do
    jal <- reflect algo
    jmv <- reflect mv

    jgs <- [java| brgt.GameInterface.playerMove($jal, $jmv) |]
    gs <- readGameState jgs
    pure gs

main :: IO ()
main = do
    -- setuo JVM with access to own classes
    r <- Runfiles.create
    let jarPath = Runfiles.rlocation r "bridget_ui_ws/jar_deploy.jar"
        jvmArgs = [ "-Djava.class.path=" <> fromString jarPath ]
    withJVM jvmArgs $ do
        st <- reflect (DT.pack "HENLO")
        handle (showException >=> DTIO.putStrLn) [java| { 
                    System.out.println($st);
                } 
            |]

        -- backend server for bridget UI
        liftIO $ scotty 3000 $ do
            middleware simpleCors
            get "/" $ do
                html $ "<h1>Welcome To bridget UI backend!</h1>" <>
                       "<p>try browsing /move/L 23 0 7 to place a block.</p>" <> 
                       "<p>(piece type (T/L/Z/O), rotation index (0-23), x (0-7), y (0-7))</p>"

            get "/move/:mvstr" $ do
                mvstr <- pathParam "mvstr"
                liftIO $ putStrLn ("haskell got command: " <> mvstr)

                gs <- makeMove (DT.pack "MINIMAX") (DT.pack mvstr)

                json gs