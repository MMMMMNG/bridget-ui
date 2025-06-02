module BridgetPage exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Json.Decode as Decode
import Task
import Update exposing (..)

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
      , gameState = Nothing
      , showInvalid = showInvalidInit
      , inventory = initialInventory
      , pieceSelectDisabled = False
      , aiThinking = False
      }
    , Cmd.batch
        [ Task.perform
            (\vp -> WindowResize vp.scene.width vp.scene.height)
            Dom.getViewport
        , Task.succeed InvalidLoad |> Task.perform identity
        ]
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