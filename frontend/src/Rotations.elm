module Rotations exposing (..)

type alias Position3D = { x : Int, y : Int, z : Int }
type alias Rotation = { index : Int, positions : List Position3D }

type alias RotationGroup =
    { rotation : Rotation
    , next : { a : Int, d : Int, w : Int, s : Int, q : Int, e : Int, z : Int }
    }

-- T block:   x
--          xXx
tBlockRotationGroups : List RotationGroup
tBlockRotationGroups =
    [ { rotation = { index = 0, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=1,y=0,z=0}, {x=0,y=-1,z=0} ] }
      , next = { a = 1, d = 2, w = 3, s = 0, q = 0, e = 0, z = 1 }
      }
    , { rotation = { index = 1, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=0,y=0,z=1}, {x=0,y=-1,z=0} ] }
      , next = { a = 2, d = 3, w = 0, s = 1, q = 1, e = 1, z = 2 }
      }
    , { rotation = { index = 2, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=-1,y=0,z=0}, {x=0,y=-1,z=0} ] }
      , next = { a = 3, d = 0, w = 1, s = 2, q = 2, e = 2, z = 3 }
      }
    , { rotation = { index = 3, positions = [ {x=0,y=0,z=1}, {x=0,y=1,z=1}, {x=0,y=-1,z=1}, {x=0,y=0,z=0} ] }
      , next = { a = 0, d = 1, w = 2, s = 3, q = 3, e = 3, z = 4 }
      }

      -- X-axis rotations (4-7)
    , { rotation = { index = 4, positions = [ {x=0,y=0,z=1}, {x=0,y=0,z=0}, {x=1,y=0,z=1}, {x=0,y=0,z=2} ] }
      , next = { a = 5, d = 6, w = 7, s = 4, q = 4, e = 4, z = 5 }
      }
    , { rotation = { index = 5, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=-1,y=0,z=0}, {x=0,y=0,z=1} ] }
      , next = { a = 6, d = 7, w = 4, s = 5, q = 5, e = 5, z = 6 }
      }
    , { rotation = { index = 6, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=-1,y=0,z=0}, {x=0,y=-1,z=0} ] }
      , next = { a = 7, d = 4, w = 5, s = 6, q = 6, e = 6, z = 7 }
      }
    , { rotation = { index = 7, positions = [ {x=0,y=0,z=1}, {x=1,y=0,z=1}, {x=-1,y=0,z=1}, {x=0,y=0,z=0} ] }
      , next = { a = 4, d = 5, w = 6, s = 7, q = 7, e = 7, z = 14 }
      }

      -- Z-axis rotations (12-15)
    , { rotation = { index = 14, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=-1,y=0,z=0}, {x=0,y=1,z=0} ] }
      , next = { a = 17, d = 18, w = 19, s = 14, q = 14, e = 14, z = 17 }
      }
    , { rotation = { index = 17, positions = [ {x=0,y=0,z=1}, {x=0,y=0,z=0}, {x=0,y=1,z=1}, {x=0,y=0,z=2} ] }
      , next = { a = 18, d = 19, w = 14, s = 17, q = 17, e = 17, z = 18 }
      }
    , { rotation = { index = 18, positions = [ {x=0,y=0,z=1}, {x=0,y=0,z=0}, {x=-1,y=0,z=1}, {x=0,y=0,z=2} ] }
      , next = { a = 19, d = 14, w = 17, s = 18, q = 18, e = 18, z = 19 }
      }
    , { rotation = { index = 19, positions = [ {x=0,y=0,z=1}, {x=0,y=0,z=0}, {x=0,y=-1,z=1}, {x=0,y=0,z=2} ] }
      , next = { a = 14, d = 17, w = 18, s = 19, q = 19, e = 19, z = 0 }
      }
    ]

-- L block: Xx
--          x
--          x
lBlockRotationGroups : List RotationGroup
lBlockRotationGroups =
    [ { rotation = { index = 0, positions = [ {x=0,y=0,z=0}, {x=1,y=1,z=0}, {x=0,y=1,z=0}, {x=0,y=-1,z=0} ] }
      , next = { a = 1, d = 3, w = 16, s = 4, q = 22, e = 12, z = 1 }
      }
    , { rotation = { index = 1, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=1}, {x=0,y=-1,z=0}, {x=0,y=1,z=0} ] }
      , next = { a = 2, d = 0, w = 4, s = 5, q = 5, e = 15, z = 2 }
      }
    , { rotation = { index = 2, positions = [ {x=0,y=0,z=0}, {x=-1,y=1,z=0}, {x=0,y=-1,z=0}, {x=0,y=1,z=0} ] }
      , next = { a = 3, d = 1, w = 5, s = 6, q = 2, e = 2, z = 3 }
      }
    , { rotation = { index = 3, positions = [ {x=0,y=0,z=1}, {x= 0,y=-1,z=1}, {x=0,y=1,z=1}, {x=0,y=1,z=0} ] }
      , next = { a = 0, d = 2, w = 6, s = 7, q = 7, e = 13, z = 4 }
      }
    , { rotation = { index = 4, positions = [ {x=0,y=0,z=1}, {x= 1,y=0,z=0}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
      , next = { a = 5, d = 6, w = 0, s = 8, q = 21, e = 23, z = 5 }
      }
    , { rotation = { index = 5, positions = [ {x=0,y=0,z=0}, {x= 1,y=0,z=0}, {x=-1,y=0,z=0}, {x=-1,y=0,z=1} ] }
      , next = { a = 6, d = 7, w = 8, s = 9, q = 9, e = 1, z = 6 }
      }
    , { rotation = { index = 6, positions = [ {x=0,y=0,z=0}, {x= 1,y=0,z=0}, {x=-1,y=0,z=0}, {x=-1,y=-1,z=0} ] }
      , next = { a = 7, d = 8, w = 9, s = 10, q = 6, e = 6, z = 7 }
      }
    , { rotation = { index = 7, positions = [ {x=0,y=0,z=1}, {x= -1,y=0,z=1}, {x=1,y=0,z=1}, {x=-1,y=0,z=0} ] }
      , next = { a = 8, d = 9, w = 10, s = 11, q = 11, e = 3, z = 8 }
      }
    , { rotation = { index = 8, positions = [ {x=0,y=0,z=0}, {x= -1,y=-1,z=0}, {x=0,y=1,z=0}, {x=0,y=-1,z=0} ] }
      , next = { a = 9, d = 10, w = 11, s = 12, q = 12, e = 22, z = 9 }
      }
    , { rotation = { index = 9, positions = [ {x=0,y=0,z=0}, {x= 0,y=-1,z=1}, {x=0,y=1,z=0}, {x=0,y=-1,z=0} ] }
      , next = { a = 10, d = 11, w = 12, s = 13, q = 15, e = 5, z = 10 }
      }
    , { rotation = { index = 10, positions = [ {x=0,y=0,z=0}, {x= 1,y=-1,z=0}, {x=0,y=1,z=0}, {x=0,y=-1,z=0} ] }
      , next = { a = 11, d = 12, w = 13, s = 14, q = 10, e = 10, z = 11 }
      }
    , { rotation = { index = 11, positions = [ {x=0,y=0,z=1}, {x= 0,y=1,z=1}, {x=0,y=-1,z=1}, {x=0,y=-1,z=0} ] }
      , next = { a = 12, d = 13, w = 14, s = 15, q = 13, e = 7, z = 12 }
      }
    , { rotation = { index = 12, positions = [ {x=0,y=0,z=0}, {x=1,y=-1,z=0}, {x=-1,y=0,z=0}, {x=1,y=0,z=0} ] }
      , next = { a = 13, d = 14, w = 15, s = 16, q = 0, e = 8, z = 13 }
      }
    , { rotation = { index = 13, positions = [ {x=0,y=0,z=1}, {x=1,y=0,z=1}, {x=-1,y=0,z=1}, {x=1,y=0,z=0} ] }
      , next = { a = 14, d = 15, w = 16, s = 17, q = 3, e = 11, z = 14 }
      }
    , { rotation = { index = 14, positions = [ {x=0,y=0,z=0}, {x=1,y=1,z=0}, {x=-1,y=0,z=0}, {x=1,y=0,z=0} ] }
      , next = { a = 15, d = 16, w = 17, s = 18, q = 14, e = 14, z = 15 }
      }
    , { rotation = { index = 15, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=1}, {x=-1,y=0,z=0}, {x=1,y=0,z=0} ] }
      , next = { a = 16, d = 17, w = 18, s = 19, q = 1, e = 9, z = 16 }
      }
    , { rotation = { index = 16, positions = [ {x=0,y=0,z=1}, {x= 1,y=0,z=2}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
      , next = { a = 17, d = 18, w = 19, s = 0, q = 19, e = 17, z = 17 }
      }
    , { rotation = { index = 17, positions = [ {x=0,y=0,z=1}, {x=0,y=1,z=2}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
      , next = { a = 18, d = 19, w = 20, s = 21, q = 16, e = 18, z = 18 }
      }
    , { rotation = { index = 18, positions = [ {x=0,y=0,z=1}, {x=-1,y=0,z=2}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
      , next = { a = 19, d = 20, w = 21, s = 22, q = 17, e = 19, z = 19 }
      }
    , { rotation = { index = 19, positions = [ {x=0,y=0,z=1}, {x=0,y=-1,z=2}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
      , next = { a = 20, d = 21, w = 22, s = 23, q = 18, e = 16, z = 20 }
      }
    , { rotation = { index = 20, positions = [ {x=0,y=0,z=1}, {x= -1,y=0,z=0}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
      , next = { a = 21, d = 22, w = 23, s = 0, q = 23, e = 21, z = 21 }
      }
    , { rotation = { index = 21, positions = [ {x=0,y=0,z=1}, {x= 0,y=-1,z=0}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
      , next = { a = 22, d = 23, w = 0, s = 1, q = 20, e = 4, z = 22 }
      }
    , { rotation = { index = 22, positions = [ {x=0,y=0,z=0}, {x= 1,y=0,z=0}, {x=-1,y=0,z=0}, {x=-1,y=1,z=0} ] }
      , next = { a = 23, d = 0, w = 1, s = 2, q = 8, e = 0, z = 23 }
      }
    , { rotation = { index = 23, positions = [ {x=0,y=0,z=1}, {x= 0,y=1,z=0}, {x=0,y=0,z=0}, {x=0,y=0,z=2} ] }
      , next = { a = 0, d = 1, w = 2, s = 3, q = 4, e = 20, z = 0 }
      }
    ]

-- Z block:  xx
--          xX
zBlockRotationGroups : List RotationGroup
zBlockRotationGroups =
    [ { rotation = { index = 0, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=1,y=-1,z=0}, {x=0,y=1,z=0} ] }
      , next = { a = 1, d = 2, w = 3, s = 4, q = 0, e = 0, z = 1 }
      }
    , { rotation = { index = 1, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=0,y=0,z=1}, {x=0,y=-1,z=1} ] }
      , next = { a = 2, d = 3, w = 4, s = 5, q = 1, e = 1, z = 2 }
      }
    , { rotation = { index = 2, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=-1,y=0,z=0}, {x=-1,y=-1,z=0} ] }
      , next = { a = 3, d = 4, w = 5, s = 6, q = 2, e = 2, z = 3 }
      }
    , { rotation = { index = 3, positions = [ {x=0,y=0,z=0}, {x=0,y=-1,z=0}, {x=0,y=0,z=1}, {x=0,y=1,z=1} ] }
      , next = { a = 4, d = 5, w = 6, s = 7, q = 3, e = 3, z = 4 }
      }
    , { rotation = { index = 4, positions = [ {x=0,y=0,z=1}, {x=0,y=0,z=0}, {x=1,y=0,z=1}, {x=1,y=0,z=2} ] }
      , next = { a = 5, d = 6, w = 7, s = 8, q = 4, e = 4, z = 5 }
      }
    , { rotation = { index = 5, positions = [ {x=0,y=0,z=0}, {x=-1,y=0,z=0}, {x=0,y=0,z=1}, {x=1,y=0,z=1} ] }
      , next = { a = 6, d = 7, w = 8, s = 9, q = 5, e = 5, z = 6 }
      }
    , { rotation = { index = 6, positions = [ {x=0,y=0,z=0}, {x=-1,y=0,z=0}, {x=0,y=-1,z=0}, {x=1,y=-1,z=0} ] }
      , next = { a = 7, d = 8, w = 9, s = 10, q = 6, e = 6, z = 7 }
      }
    , { rotation = { index = 7, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=0,y=0,z=1}, {x=-1,y=0,z=1} ] }
      , next = { a = 8, d = 9, w = 10, s = 11, q = 7, e = 7, z = 12 }
      }
    , { rotation = { index = 12, positions = [ {x=0,y=0,z=0}, {x=-1,y=-1,z=0}, {x=0,y=-1,z=0}, {x=1,y=0,z=0} ] }
      , next = { a = 16, d = 17, w = 19, s = 0, q = 12, e = 12, z = 16 }
      }
    , { rotation = { index = 16, positions = [ {x=0,y=0,z=1}, {x=1,y=0,z=1}, {x=0,y=0,z=2}, {x=1,y=0,z=0} ] }
      , next = { a = 17, d = 19, w = 0, s = 1, q = 16, e = 16, z = 17 }
      }
    , { rotation = { index = 17, positions = [ {x=0,y=0,z=1}, {x=0,y=1,z=1}, {x=0,y=0,z=2}, {x=0,y=1,z=0} ] }
      , next = { a = 19, d = 0, w = 1, s = 2, q = 17, e = 17, z = 19 }
      }
    , { rotation = { index = 19, positions = [ {x=0,y=0,z=1}, {x=0,y=-1,z=1}, {x=0,y=0,z=2}, {x=0,y=-1,z=0} ] }
      , next = { a = 0, d = 1, w = 2, s = 3, q = 19, e = 19, z = 0 }
      }
    ]

-- O block: Xx
--          xx
oBlockRotationGroups : List RotationGroup
oBlockRotationGroups =
    [ { rotation = { index = 0, positions = [ {x=0,y=0,z=0}, {x=1,y=0,z=0}, {x=0,y=1,z=0}, {x=1,y=1,z=0} ] }
      , next = { a = 4, d = 4, w = 1, s = 0, q = 0, e = 0, z = 1 }
      }
    , { rotation = { index = 1, positions = [ {x=0,y=0,z=0}, {x=0,y=1,z=0}, {x=0,y=0,z=1}, {x=0,y=1,z=1} ] }
      , next = { a = 4, d = 0, w = 1, s = 0, q = 4, e = 1, z = 4 }
      }
    , { rotation = { index = 4, positions = [ {x=0,y=0,z=0}, {x=0,y=0,z=1}, {x=1,y=0,z=0}, {x=1,y=0,z=1} ] }
      , next = { a = 4, d = 0, w = 4, s = 4, q = 4, e = 1, z = 0 }
      }
    ]