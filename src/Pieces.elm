module Pieces exposing (..)

import Scene3d
import Length
import Point3d
import Scene3d.Material as Material
import Color
type WorldCoordinates
    = WorldCoordinates
{-| Create a cube entity by constructing six square faces with different colors
-}
initialCube : Scene3d.Entity WorldCoordinates
initialCube =
    let
        -- Define the negative and positive X/Y/Z coordinates of a 16 'pixel'
        -- wide cube centered at the origin (see https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length#cssPixels)
        negative =
            Length.meters -0.25

        positive =
            Length.meters 0.25

        -- Define the eight vertices of the cube
        p1 =
            Point3d.xyz negative negative negative

        p2 =
            Point3d.xyz positive negative negative

        p3 =
            Point3d.xyz positive positive negative

        p4 =
            Point3d.xyz negative positive negative

        p5 =
            Point3d.xyz negative negative positive

        p6 =
            Point3d.xyz positive negative positive

        p7 =
            Point3d.xyz positive positive positive

        p8 =
            Point3d.xyz negative positive positive

        -- Create the six faces with different colors
        bottom =
            Scene3d.quad (Material.color Color.blue) p1 p2 p3 p4

        top =
            Scene3d.quad (Material.color Color.gray) p5 p6 p7 p8

        front =
            Scene3d.quad (Material.color Color.orange) p2 p3 p7 p6

        back =
            Scene3d.quad (Material.color Color.orange) p1 p4 p8 p5

        left =
            Scene3d.quad (Material.color Color.green) p1 p2 p6 p5

        right =
            Scene3d.quad (Material.color Color.green) p4 p3 p7 p8
    in
    -- Combine all faces into a single entity
    Scene3d.group [ bottom, top, front, back, left, right ]