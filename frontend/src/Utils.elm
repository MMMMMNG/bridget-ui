module Utils exposing (..)
import Json.Decode as Decode
import Types exposing (PieceType)
import Rotations
import Types exposing (..)
import Constants exposing (boardSize, boardHeight)

getCubeOffsets : PieceType -> Int -> List Rotations.Position3D
getCubeOffsets pieceType rotIndex =
    let
        rotationGroups =
            case pieceType of
                LShape -> Rotations.lBlockRotations
                TShape -> Rotations.tBlockRotations
                ZShape -> Rotations.zBlockRotations
                OShape -> Rotations.oBlockRotations
    in
    case List.drop rotIndex rotationGroups |> List.head of
        Just group -> group.positions
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
        _ -> NoOp


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
        "z"          -> RotatePieceKey "z"
        "Enter"      -> PlacePiece
        _            -> NoOp

pTs : PieceType -> String
pTs pt = case pt of
    LShape -> "L"
    OShape -> "O"
    ZShape -> "Z"
    TShape -> "T"

keepPieceInBounds : PieceType -> Int -> Int -> Int -> Int -> (Int, Int, Int)
keepPieceInBounds pieceType newRotIndex pieceX pieceY pieceZ =
    let
        cubeOffsets = getCubeOffsets pieceType newRotIndex
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

        newX = clamp minX maxX pieceX
        newY = clamp minY maxY pieceY
        newZ = clamp minZ maxZ pieceZ
    in (newX, newY, newZ)