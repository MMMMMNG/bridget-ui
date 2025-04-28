module Main exposing (main)

{-| Just about the simplest elm-3d-scene program! This example introduces
several fundamental concepts used in elm-3d-scene:

  - Creating an entity to draw
  - Defining a camera
  - Rendering a scene to create an HTML element

-}

import Angle exposing (Angle)
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d exposing (Viewpoint3d)
import Quantity exposing (Quantity)
import Pixels exposing (Pixels)
import Browser
import Browser.Events
import Json.Decode as Decode exposing (Decoder)

type alias Model =
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , orbiting : Bool -- Whether the mouse button is currently down
    }

type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)


init : () -> ( Model, Cmd Msg )
init () =
    ( { azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        -- Start orbiting when a mouse button is pressed
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        -- Orbit camera on mouse move (if a mouse button is down)
        MouseMove dx dy ->
            if model.orbiting then
                let
                    -- How fast we want to orbit the camera (orbiting the
                    -- camera by 1 degree per pixel of drag is a decent default
                    -- to start with)
                    rotationRate =
                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                    -- Adjust azimuth based on horizontal mouse motion (one
                    -- degree per pixel)
                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    -- Adjust elevation based on vertical mouse motion (one
                    -- degree per pixel), and clamp to make sure camera cannot
                    -- go past vertical in either direction
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        -- If we're currently orbiting, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]

    else
        -- If we're not currently orbiting, just listen for mouse down events
        -- to start orbiting
        Browser.Events.onMouseDown (Decode.succeed MouseDown)


view : Model -> Browser.Document Msg
view model =
    let
        -- Create a single rectangle from its color and four vertices
        -- (Scene3d.quad can be used to create any flat four-sided shape)
        square =
            Scene3d.quad (Material.color Color.blue)
                (Point3d.meters -1 -1 0)
                (Point3d.meters 1 -1 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters -1 1 0)

        -- Define our camera
        camera : Camera3d.Camera3d Length.Meters coordinates
        camera =
        -- We want to use perspective (realistic) projection
        -- with a total vertical field of view of 30 degrees
            Camera3d.perspective
                -- Camera is at the point (4, 2, 2),                             
                -- looking at the point (0, 0, 0),                             
                -- with positive Z as the 'up' direction                             
                { viewpoint = Viewpoint3d.lookAt 
                    { eyePoint = Point3d.meters 4 2 2                            
                     ,focalPoint = Point3d.origin
                     ,upDirection = Direction3d.positiveZ
                    }
                 ,verticalFieldOfView = Angle.degrees 30
                }
    in
    { title = "OrbitingCamera"
    , body =
        [ Scene3d.unlit
            { -- Our scene has a single 'entity' in it
              entities = [ square ]

            -- Provide the camera to be used when rendering the scene
            , camera = camera

            -- Anything closer than 1 meter to the camera will be clipped away
            -- (this is necessary because of the internals of how WebGL works)
            , clipDepth = Length.meters 1

            -- Using a transparent background means that the HTML underneath the
            -- scene will show through
            , background = Scene3d.transparentBackground

            -- Size in pixels of the generated HTML element
            , dimensions = ( Pixels.int 400, Pixels.int 300 )
            }
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }