module Utils exposing (getCubeOffsets, mouseDecoder, keyCheck, keyToMsg, pTs)
import Json.Decode as Decode
import Types exposing (PieceType)
import Rotations
import Types exposing (..)

getCubeOffsets : PieceType -> Int -> List Rotations.Position3D
getCubeOffsets pieceType rotIndex =
    let
        rotationGroups =
            case pieceType of
                LShape -> Rotations.lBlockRotationGroups
                TShape -> Rotations.tBlockRotationGroups
                ZShape -> Rotations.zBlockRotationGroups
                OShape -> Rotations.oBlockRotationGroups
    in
    case List.drop rotIndex rotationGroups |> List.head of
        Just group -> group.rotation.positions
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
    let
        key =
            case Decode.decodeValue (Decode.field "key" Decode.string) json of
                Ok k -> k
                Err _ -> ""
    in
    case key of
        "ArrowLeft"  -> MovePiece 0 -1
        "ArrowRight" -> MovePiece 0 1
        "ArrowUp"    -> MovePiece -1 0
        "ArrowDown"  -> MovePiece 1 0
        "a"          -> RotatePieceKey "a"
        "d"          -> RotatePieceKey "d"
        "w"          -> RotatePieceKey "w"
        "s"          -> RotatePieceKey "s"
        "q"          -> RotatePieceKey "q"
        "e"          -> RotatePieceKey "e"
        _            -> NoOp

pTs : PieceType -> String
pTs pt = case pt of
    LShape -> "L"
    OShape -> "O"
    ZShape -> "Z"
    TShape -> "T"