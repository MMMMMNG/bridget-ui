module Utils exposing (getCubeOffsets, mouseDecoder, keyCheck, keyToMsg, pTs)
import Json.Decode as Decode
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

mouseDecoder : (Float -> Float -> Msg) -> Decode.Decoder Msg
mouseDecoder msg =
    Decode.map2 msg
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


keyCheck : Bool -> Decode.Value -> Msg
keyCheck isDown json =
    case Decode.decodeValue (Decode.field "key" Decode.string) json of
        Ok "Shift" ->
            KeyChanged isDown

        _ ->
            NoOp


keyToMsg : Decode.Value -> Msg
keyToMsg json =
    case Decode.decodeValue (Decode.field "key" Decode.string) json of
        Ok "ArrowLeft"    -> MovePiece 0 -1
        Ok "ArrowRight"  -> MovePiece 0 1
        Ok "ArrowUp"  -> MovePiece -1 0
        Ok "ArrowDown" -> MovePiece 1 0
        Ok "d"          -> RotatePiece Z 1
        Ok "a"          -> RotatePiece Z -1
        _               -> NoOp


pTs : PieceType -> String
pTs pt = case pt of
    LShape -> "L"
    OShape -> "O"
    ZShape -> "Z"
    TShape -> "T"