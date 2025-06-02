module UpdatePieceRotation exposing (..)
import Rotations exposing (Rotation, Position3D, tBlockRotations, lBlockRotations, zBlockRotations, oBlockRotations)
import Set
import Types exposing (..)
import Utils exposing (getCubeOffsets)
import Constants exposing (boardSize, boardHeight)
import List.Extra
-- basic concept: use a given rot mat for each key (w a s d), apply it to the piece, then
-- "normalize" the piece-translation by moving it into the positive octant
-- or if it is already in the positive octant, towards zero. Then the piece is "tucked"
-- into the positive octants corner. do the same for every hardcoded rotation and then
-- search the rotation array for the corresponding rotation, therby finding which
-- rotation index the current rotation has.



-- to "tuck a piece in" I basically need to compute the bounding cuboid of the piece and then
-- move its corner to the origin.


{-| update the rotation of the current piece after e.g. a keypress
-}
updatePiceRotation : String -> Model -> ( Model, Cmd Msg )
updatePiceRotation keyStr model =
            let
                rotationGroups =
                    case model.pieceType of
                        LShape -> Rotations.lBlockRotations
                        TShape -> Rotations.tBlockRotations
                        ZShape -> Rotations.zBlockRotations
                        OShape -> Rotations.oBlockRotations

                -- Find the current group by list index
                currentGroup =
                    List.drop model.pieceRotIndex rotationGroups |> List.head

                -- Find the group in the list whose .rotation.index matches the target index
                findGroupByRotationIndex idx =
                    List.Extra.find (\g -> g.index == idx) rotationGroups

                newRotIndex = case currentGroup of
                    Just cg -> rotByKey keyStr cg
                    _ -> 0 --hmm

                cubeOffsets = getCubeOffsets model.pieceType newRotIndex
                minDx = List.minimum (List.map (\p -> p.x) cubeOffsets) |> Maybe.withDefault 0
                maxDx = List.maximum (List.map (\p -> p.x) cubeOffsets) |> Maybe.withDefault 0
                minDy = List.minimum (List.map (\p -> p.y) cubeOffsets) |> Maybe.withDefault 0
                maxDy = List.maximum (List.map (\p -> p.y) cubeOffsets) |> Maybe.withDefault 0
                minDz = List.minimum (List.map (\p -> p.z) cubeOffsets) |> Maybe.withDefault 0
                maxDz = List.maximum (List.map (\p -> p.z) cubeOffsets) |> Maybe.withDefault 0

                minX = 0 - minDx
                maxX = boardSize - 1 - maxDx
                minY = 0 - minDy
                maxY = boardSize - 1 - maxDy
                minZ = 0 - minDz
                maxZ = boardHeight - 1 - maxDz

                newX = clamp minX maxX model.pieceX
                newY = clamp minY maxY model.pieceY
                newZ = clamp minZ maxZ model.pieceZ
            in
            ( { model
                | pieceRotIndex = newRotIndex
                , pieceX = newX
                , pieceY = newY
                , pieceZ = newZ
              }
            , Cmd.none
            )

{-| Applies a 3x3 rotation matrix to each position in the GameObject's rotation data.
    The matrix is expected to be a List of 3 Lists of 3 Floats.
-}
applyRotMat : List (List Int) -> Rotation -> Rotation
applyRotMat rotMatrix gameObject =
    let
        rotatedPositions =
            List.map (applyMatrixToPosition rotMatrix) gameObject.positions
    in
    { gameObject | positions = rotatedPositions }


{-| Applies a 3x3 matrix to a single Position.
    Assumes the matrix is valid (3x3).
    The position (x, y, z) is treated as a column vector.
-}
applyMatrixToPosition : List (List Int) -> Position3D -> Position3D
applyMatrixToPosition rotMatrix position =
    let
        -- Treat position as a column vector [x, y, z]
        vector = [ position.x, position.y, position.z ]

        -- Perform matrix-vector multiplication
        -- For each row in the matrix, compute the dot product with the vector
        newCoordinates = List.map (\row -> dotProduct row vector) rotMatrix
    in case newCoordinates of
        [ newX, newY, newZ ] -> { x = newX, y = newY, z = newZ }
        _ -> Debug.todo "should be unreachable?"


{-| Computes the dot product of two lists of Floats.
    Assumes lists are of the same length.
-}
dotProduct : List Int -> List Int -> Int
dotProduct listA listB =
    List.map2 (*) listA listB
        |> List.sum


rotMatZcw : List (List Int)
rotMatZcw = 
    [ [ 0, 1, 0 ]
    , [ -1, 0, 0 ]
    , [ 0, 0, 1 ]
    ]
rotMatZccw : List (List Int)
rotMatZccw =
    [ [ 0, -1, 0 ]
    , [ 1, 0, 0 ]
    , [ 0, 0, 1 ]
    ]
rotMatYccw : List (List Int)
rotMatYccw =
    [ [ 0, 0, 1 ]
    , [ 0, 1, 0 ]
    , [ -1, 0, 0 ]
    ]
rotMatYcw : List (List Int)
rotMatYcw =
    [ [ 0, 0, -1 ]
    , [ 0, 1, 0 ]
    , [ 1, 0, 0 ]
    ]

rotMatIdent = 
    [ [ 1, 0, 0 ]
    , [ 0, 1, 0 ]
    , [ 0, 0, 1 ]
    ]

{-| take the current rotationgroup and the pressed key string,
    and return the correct new rotation index.
-}
rotByKey : String -> Rotation -> Int
rotByKey keyStr rg = 
    let rm = 
            case keyStr of
                "a" -> rotMatZcw
                "d" -> rotMatZccw
                "w" -> rotMatYcw
                "s" -> rotMatYccw
                _   -> rotMatIdent -- noop

        rotatedRG = applyRotMat rm rg
        
    in findNewRotIndexFromRotatedPiece rotatedRG


{-| Checks if two RotationGroup objects have the same positions using Sets,
    meaning duplicate positions within a single list are treated as a single occurrence.
    this shouldn't matter because no shape in bridget as duplicate positions (how could it?)
-}
haveSamePositionsSet : Rotation -> Rotation -> Bool
haveSamePositionsSet group1 group2 =
    let
        -- Helper to convert Position3D to a comparable tuple
        positionToTuple : Position3D -> ( Int, Int, Int )
        positionToTuple pos = ( pos.x, pos.y, pos.z )

        positions1 = List.map positionToTuple group1.positions
        positions2 = List.map positionToTuple group2.positions
        -- Convert lists to Sets. This naturally handles uniqueness and "order".
        set1 = Set.fromList positions1
        set2 = Set.fromList positions2
    in
    -- Compare the Sets for equality
    set1 == set2

{-| Finds the minimum x, y, and z values across all positions in a RotationGroup.
    Returns Nothing if the list of positions is empty.
-}
findMinExtents : Rotation -> Maybe Position3D
findMinExtents group =
    case group.positions of
        [] ->
            Nothing

        firstPosition :: restOfPositions ->
            let
                -- Reducer function to find minimums
                updateMins : Position3D -> Position3D -> Position3D
                updateMins pos currentMins =
                    { x = min currentMins.x pos.x
                    , y = min currentMins.y pos.y
                    , z = min currentMins.z pos.z
                    }
            in
            Just (List.foldl updateMins firstPosition restOfPositions)


{-| Shifts all positions in a RotationGroup so that the minimum x, y, and z values
    become 0, effectively moving the object to the positive octant with a corner at the origin.
    If the rotation group has no positions, it returns the group unchanged.
-}
tuckIntoPositiveOctant : Rotation -> Rotation
tuckIntoPositiveOctant group =
    case findMinExtents group of
        Nothing -> group -- If there are no positions, nothing to shift.
            
        Just minPos ->
            let -- The translation vector is the negative of the minimums
                -- (dx, dy, dz) = (-minX, -minY, -minZ)
                -- Helper function to apply the translation to a single position
                applyTranslation : Position3D -> Position3D
                applyTranslation pos =
                    { x = pos.x - minPos.x
                    , y = pos.y - minPos.y
                    , z = pos.z - minPos.z
                    }

                -- Apply the translation to all positions
                shiftedPositions = List.map applyTranslation group.positions
            in
            -- Return a new RotationGroup with the shifted positions
            { group | positions = shiftedPositions }

{-| given a rotated piece, this function finds the first match in any
    of the rotationgroup lists and outputs its index.
-}
findNewRotIndexFromRotatedPiece : Rotation -> Int
findNewRotIndexFromRotatedPiece rotatedRg =
    let -- we gotta tuck RGs before comparing 'em (normalization)
        -- because the rotation might've changed relative translations
        tuckedRg = tuckIntoPositiveOctant rotatedRg

        -- helper function, checks if a rotationgroup matches our rotated RG
        check : Rotation -> Bool
        check rg = 
            let --can't forget to tuck our contender, gotta be normalized too
                tuckedRGtoBeChecked = tuckIntoPositiveOctant rg
                --finally check if this is our RG
            in haveSamePositionsSet tuckedRg tuckedRGtoBeChecked

        --helper function, find first matching rotation
        findFirst : List Rotation -> Int
        findFirst lst = case lst of
            []      -> 0 --just default to rotindex 0.
            l :: ls -> if check l 
                then l.index
                else findFirst ls
        
        --concat all RGs into one huge list
        allRGs = List.concat 
            [   tBlockRotations
            ,   lBlockRotations
            ,   zBlockRotations
            ,   oBlockRotations
            ]
    in findFirst allRGs
