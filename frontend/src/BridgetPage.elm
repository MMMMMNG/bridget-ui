module BridgetPage exposing (main)

import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events
import Color
import Frame3d
import Json.Decode as Decode
import Length
import Point3d
import Scene3d
import Scene3d.Material as Material
import Task
import Rotations
import Update exposing (..)

import Json.Decode as D
import Url.Builder
import Http
import Types exposing (..)
import View exposing (view)
import Utils exposing (mouseDecoder, keyCheck, keyToMsg)

-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    let
        showInvalidInit = False
        initialInventory =
            { l = 4, z = 4, t = 4, o = 2 }
    in
    ( { azimuth = 45
      , elevation = 30
      , isMouseDown = False
      , lastMousePos = Nothing
      , windowWidth = 0
      , windowHeight = 0
      , shiftKeyPressed = False
      , pieceRotIndex = 0
      , pieceX = 3
      , pieceY = 3
      , pieceZ = 0
      , pieceType = LShape
      , currentPlayer = Player1
      , gameState = Nothing
      , showInvalid = showInvalidInit
      , inventory = initialInventory
      , pieceSelectDisabled = False
      }
    , Task.perform
        (\vp -> WindowResize vp.scene.width vp.scene.height)
        Dom.getViewport
    )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseDown (mouseDecoder MouseDown)
        , Browser.Events.onMouseUp (Decode.succeed MouseUp)
        , Browser.Events.onResize (\w h -> WindowResize (toFloat w) (toFloat h))
        , Browser.Events.onKeyDown (Decode.map keyToMsg Decode.value)
        , Browser.Events.onKeyUp (Decode.map (keyCheck False) Decode.value)
        , if model.isMouseDown then
            Browser.Events.onMouseMove (mouseDecoder MouseMove)

          else
            Sub.none
        ]



-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }