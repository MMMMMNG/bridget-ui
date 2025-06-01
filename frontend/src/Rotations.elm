module Rotations exposing (..)
type alias Position3D = { x : Int, y : Int, z : Int }
type alias Rotation = { index : Int, positions : List Position3D }

-- T block:   x
--          xXx
tBlockRotations : List Rotation
tBlockRotations =
    [ { index = 0, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=1,y=0,z=0}, {x=0,y=-1,z=0} ] }
    , { index = 1, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=0,y=0,z=1}, {x=0,y=-1,z=0} ] }
    , { index = 2, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=-1,y=0,z=0}, {x=0,y=-1,z=0} ] }
    , { index = 3, positions = [ {x=0,y=0,z=1}, {x=0,y=1,z=1}, {x=0,y=-1,z=1}, {x=0,y=0,z=0} ] }

      -- X-axis rotations (4-7)
    , { index = 4, positions = [ {x=0,y=0,z=1}, {x=0,y=0,z=0}, {x=1,y=0,z=1}, {x=0,y=0,z=2} ] }
    , { index = 5, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=-1,y=0,z=0}, {x=0,y=0,z=1} ] }
    , { index = 6, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=-1,y=0,z=0}, {x=0,y=-1,z=0} ] }
    , { index = 7, positions = [ {x=0,y=0,z=1}, {x=1,y=0,z=1}, {x=-1,y=0,z=1}, {x=0,y=0,z=0} ] }

      -- Z-axis rotations (12-15)
    , { index = 14, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=-1,y=0,z=0}, {x=0,y=1,z=0} ] }
    , { index = 17, positions = [ {x=0,y=0,z=1}, {x=0,y=0,z=0}, {x=0,y=1,z=1}, {x=0,y=0,z=2} ] }
    , { index = 18, positions = [ {x=0,y=0,z=1}, {x=0,y=0,z=0}, {x=-1,y=0,z=1}, {x=0,y=0,z=2} ] }
    , { index = 19, positions = [ {x=0,y=0,z=1}, {x=0,y=0,z=0}, {x=0,y=-1,z=1}, {x=0,y=0,z=2} ] }
    ]

-- L block: Xx
--          x
--          x
lBlockRotations : List Rotation
lBlockRotations =
    [ { index = 0, positions = [ {x=0,y=0,z=0}, {x=1,y=1,z=0}, {x=0,y=1,z=0}, {x=0,y=-1,z=0} ] }
    , { index = 1, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=1}, {x=0,y=-1,z=0}, {x=0,y=1,z=0} ] }
    , { index = 2, positions = [ {x=0,y=0,z=0}, {x=-1,y=1,z=0}, {x=0,y=-1,z=0}, {x=0,y=1,z=0} ] }
    , { index = 3, positions = [ {x=0,y=0,z=1}, {x= 0,y=-1,z=1}, {x=0,y=1,z=1}, {x=0,y=1,z=0} ] }

      -- X-axis rotations (4-7)
    , { index = 4, positions = [ {x=0,y=0,z=1}, {x= 1,y=0,z=0}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
    , { index = 5, positions = [ {x=0,y=0,z=0}, {x= 1,y=0,z=0}, {x=-1,y=0,z=0}, {x=-1,y=0,z=1} ] }
    , { index = 6, positions = [ {x=0,y=0,z=0}, {x= 1,y=0,z=0}, {x=-1,y=0,z=0}, {x=-1,y=-1,z=0} ] }
    , { index = 7, positions = [ {x=0,y=0,z=1}, {x= -1,y=0,z=1}, {x=1,y=0,z=1}, {x=-1,y=0,z=0} ] }

      -- Y-axis rotations (8-11)
    , { index = 8, positions = [ {x=0,y=0,z=0}, {x= -1,y=-1,z=0}, {x=0,y=1,z=0}, {x=0,y=-1,z=0} ] }
    , { index = 9, positions = [ {x=0,y=0,z=0}, {x= 0,y=-1,z=1}, {x=0,y=1,z=0}, {x=0,y=-1,z=0} ] }
    , { index = 10, positions = [ {x=0,y=0,z=0}, {x= 1,y=-1,z=0}, {x=0,y=1,z=0}, {x=0,y=-1,z=0} ] }
    , { index = 11, positions = [ {x=0,y=0,z=1}, {x= 0,y=1,z=1}, {x=0,y=-1,z=1}, {x=0,y=-1,z=0} ] }

      -- Z-axis rotations (12-15)
    , { index = 12, positions = [ {x=0,y=0,z=0}, {x=1,y=-1,z=0}, {x=-1,y=0,z=0}, {x=1,y=0,z=0} ] }
    , { index = 13, positions = [ {x=0,y=0,z=1}, {x=1,y=0,z=1}, {x=-1,y=0,z=1}, {x=1,y=0,z=0} ] }
    , { index = 14, positions = [ {x=0,y=0,z=0}, {x=1,y=1,z=0}, {x=-1,y=0,z=0}, {x=1,y=0,z=0} ] }
    , { index = 15, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=1}, {x=-1,y=0,z=0}, {x=1,y=0,z=0} ] }

      -- Diagonal rotations (16-19)
    , { index = 16, positions = [ {x=0,y=0,z=1}, {x= 1,y=0,z=2}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
    , { index = 17, positions = [ {x=0,y=0,z=1}, {x=0,y=1,z=2}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
    , { index = 18, positions = [ {x=0,y=0,z=1}, {x=-1,y=0,z=2}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
    , { index = 19, positions = [ {x=0,y=0,z=1}, {x=0,y=-1,z=2}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }

      -- Final rotations (20-23)
    , { index = 20, positions = [ {x=0,y=0,z=1}, {x= -1,y=0,z=0}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
    , { index = 21, positions = [ {x=0,y=0,z=1}, {x= 0,y=-1,z=0}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
    , { index = 22, positions = [ {x=0,y=0,z=0}, {x= 1,y=0,z=0}, {x=-1,y=0,z=0}, {x=-1,y=1,z=0} ] }
    , { index = 23, positions = [ {x=0,y=0,z=1}, {x= 0,y=1,z=0}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
    ]

-- Z block:  xx
--          xX
zBlockRotations : List Rotation
zBlockRotations =
    [ { index = 0, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=1,y=-1,z=0}, {x=0,y=1,z=0} ] }
    , { index = 1, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=0,y=0,z=1}, {x=0,y=-1,z=1} ] }
    , { index = 2, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=-1,y=0,z=0}, {x=-1,y=-1,z=0} ] }
    , { index = 3, positions = [ {x=0,y=0,z=0}, {x=0,y=-1,z=0}, {x=0,y=0,z=1}, {x=0,y=1,z=1} ] }

      -- X-axis rotations (4-7)
    , { index = 4, positions = [ {x=0,y=0,z=1}, {x=0,y=0,z=0}, {x=1,y=0,z=1}, {x=1,y=0,z=2} ] }
    , { index = 5, positions = [ {x=0,y=0,z=0}, {x=-1,y=0,z=0}, {x=0,y=0,z=1}, {x=1,y=0,z=1} ] }
    , { index = 6, positions = [ {x=0,y=0,z=0}, {x=-1,y=0,z=0}, {x=0,y=-1,z=0}, {x=1,y=-1,z=0} ] }
    , { index = 7, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=0,y=0,z=1}, {x=-1,y=0,z=1} ] }

      -- Z-axis rotations (12-15)
    , { index = 12, positions = [ {x=0,y=0,z=0}, {x=-1,y=-1,z=0}, {x=0,y=-1,z=0}, {x=1,y=0,z=0} ] }

      -- Diagonal rotations (16-19)
    , { index = 16, positions = [ {x=0,y=0,z=1}, {x=1,y=0,z=1}, {x=0,y=0,z=2}, {x=1,y=0,z=0} ] }
    , { index = 17, positions = [ {x=0,y=0,z=1}, {x=0,y=1,z=1}, {x=0,y=0,z=2}, {x=0,y=1,z=0} ] }
    , { index = 19, positions = [ {x=0,y=0,z=1}, {x=0,y=-1,z=1}, {x=0,y=0,z=2}, {x=0,y=-1,z=0} ] }
    ]

-- O block: Xx
--          xx
oBlockRotations : List Rotation
oBlockRotations =
    [ { index = 0, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=0,y=1,z=0}, {x=1,y=1,z=0} ] }
    , { index = 1, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=0,y=0,z=1}, {x=0,y=1,z=1} ] }
    , { index = 4, positions = [ {x=0,y=0,z=0}, {x=0,y=0,z=1}, {x=1,y=0,z=0}, {x=1,y=0,z=1} ] }
    ]