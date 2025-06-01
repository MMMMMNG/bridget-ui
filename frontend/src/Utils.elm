module Utils exposing (getCubeOffsets)
import Types exposing (PieceType)
import Rotations
import Types exposing (..)

getCubeOffsets : PieceType -> Int -> List Rotations.Position3D
getCubeOffsets pieceType rotIndex =
    let
        rotations =
            case pieceType of
                LShape -> Rotations.lBlockRotations
                TShape -> Rotations.tBlockRotations
                ZShape -> Rotations.zBlockRotations
                OShape -> Rotations.oBlockRotations
    in
    case List.drop rotIndex rotations |> List.head of
        Just rotation -> rotation.positions
        Nothing -> []