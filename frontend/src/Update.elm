module Update exposing (update)

import Types exposing (..)
import Constants exposing (boardSize, boardHeight)
import Rotations
import Utils exposing (getCubeOffsets, pTs, keepPieceInBounds)
import Task
import Json.Decode as D
import Http
import Url.Builder
import Process
import UpdatePieceRotation exposing (updatePiceRotation)

submitMove : PieceType -> Int -> Int -> Int -> Cmd Msg
submitMove piece rotIndex x y =
    let
        actualRotIndex =
            let
                rotationGroups =
                    case piece of
                        LShape -> Rotations.lBlockRotations
                        TShape -> Rotations.tBlockRotations
                        ZShape -> Rotations.zBlockRotations
                        OShape -> Rotations.oBlockRotations
            in
            case List.drop rotIndex rotationGroups |> List.head of
                Just group -> group.index
                Nothing -> rotIndex

        command = String.join " " [pTs piece,
                                   String.fromInt actualRotIndex,
                                   String.fromInt x,
                                   String.fromInt y]
        u = Url.Builder.crossOrigin "http://localhost:3000" [ "move", command ] []
    in Http.get
        { url = u
        , expect = gsExpecter
        }

gsExpecter : Http.Expect Msg
gsExpecter = Http.expectJson PlacePieceHttpResult gsDecoder

gsDecoder : D.Decoder (Maybe GameState)
gsDecoder =
    -- Fix: decode the board field as a string, then decode the JSON string inside it
    let
        boardStringDecoder : D.Decoder (List (List (List Int)))
        boardStringDecoder =
            D.field "board" D.string
                |> D.andThen
                    (\str ->
                        case D.decodeString (D.list (D.list (D.list D.int))) str of
                            Ok val -> D.succeed val
                            Err _ -> D.fail "Could not decode board JSON string"
                    )
    in
    D.oneOf
        [ D.map4 (\isGameOver isLastMoveInvalid winner board ->
                Just { gameOver = isGameOver
                     , moveInvalid = isLastMoveInvalid
                     , winner = winner
                     , board = board
                     }
            )
            (D.field "isGameOver" D.bool)
            (D.field "isLastMoveInvalid" D.bool)
            (D.field "winner" D.string)
            boardStringDecoder
        , D.succeed Nothing
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InvalidLoad ->
            let
                command = "-1 -1 -1 -1"
                u = Url.Builder.crossOrigin "http://localhost:3000" [ "move", command ] []
            in
            ( { model | aiThinking = True, showInvalid = False }
            , Http.get
                { url = u
                , expect = gsExpecter
                }
            )

        MouseDown x y ->
            ( { model
                | isMouseDown = True
                , lastMousePos = Just (x, y)
              }
            , Cmd.none
            )

        MouseUp ->
            ( { model
                | isMouseDown = False
                , lastMousePos = Nothing
              }
            , Cmd.none
            )

        MouseMove x y ->
            case ( model.isMouseDown, model.lastMousePos ) of
                ( True, Just ( lastX, lastY ) ) ->
                    let
                        deltaX = x - lastX
                        deltaY = y - lastY
                        sensitivity = 0.3
                        newAzimuth = model.azimuth + deltaX * sensitivity
                        newElevation = model.elevation - deltaY * sensitivity |> clamp -80 80
                    in
                    ( { model
                        | azimuth = newAzimuth
                        , elevation = newElevation
                        , lastMousePos = Just ( x, y )
                      }
                    , Cmd.none
                    )
                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        WindowResize w h ->
            ( { model
                | windowWidth = w
                , windowHeight = h
              }
            , Cmd.none
            )

        KeyChanged isPressed ->
            ( { model | shiftKeyPressed = isPressed }, Cmd.none )

        MovePiece dx dy ->
            let
                (x, y) = (model.pieceX + dx, model.pieceY + dy)
                (newX, newY, newZ) = 
                    keepPieceInBounds model.pieceType model.pieceRotIndex x y model.pieceZ
            in
            ( { model | pieceX = newX, pieceY = newY, pieceZ = newZ }, Cmd.none )

        
        PlacePieceHttpResult (Err _) -> 
            ( { model | aiThinking = False }, Cmd.none )

        PlacePieceHttpResult (Ok Nothing) -> 
            ( { model | aiThinking = False }, Cmd.none )

        PlacePieceHttpResult (Ok (Just gs)) ->
            let
                newGameState = Just { gs | board = List.map identity gs.board }
                showInvalid = gs.moveInvalid
                -- Helper to get next available piece type
                nextAvailablePiece : Inventory -> PieceType -> PieceType
                nextAvailablePiece inv current =
                    let
                        order = [ LShape, TShape, ZShape, OShape ]

                        dropWhile : (a -> Bool) -> List a -> List a
                        dropWhile pred lst =
                            case lst of
                                [] -> []
                                x :: xs ->
                                    if pred x then dropWhile pred xs else lst

                        takeWhile : (a -> Bool) -> List a -> List a
                        takeWhile pred lst =
                            case lst of
                                [] -> []
                                x :: xs ->
                                    if pred x then x :: takeWhile pred xs else []

                        rotated =
                            case dropWhile ((/=) current) order of
                                [] -> order
                                (_ :: xs) -> xs ++ takeWhile ((/=) current) order

                        findAvailableHelper lst =
                            case lst of
                                [] ->
                                    current
                                p :: ps ->
                                    let
                                        count =
                                            case p of
                                                LShape -> inv.l
                                                TShape -> inv.t
                                                ZShape -> inv.z
                                                OShape -> inv.o
                                    in
                                    if count > 0 then p else findAvailableHelper ps
                    in
                    if
                        (case current of
                            LShape -> inv.l
                            TShape -> inv.t
                            ZShape -> inv.z
                            OShape -> inv.o
                        ) > 0
                    then current
                    else findAvailableHelper rotated

                updatedInventory =
                    if not gs.moveInvalid then
                        case model.inventory of
                            { l, z, t, o } ->
                                case model.pieceType of
                                    LShape ->
                                        { l = max 0 (l - 1), z = z, t = t, o = o }
                                    ZShape ->
                                        { l = l, z = max 0 (z - 1), t = t, o = o }
                                    TShape ->
                                        { l = l, z = z, t = max 0 (t - 1), o = o }
                                    OShape ->
                                        { l = l, z = z, t = t, o = max 0 (o - 1) }
                    else
                        model.inventory

                -- After inventory update, pick next available piece if current is 0
                nextPieceType =
                    if not gs.moveInvalid then
                        nextAvailablePiece updatedInventory model.pieceType
                    else
                        model.pieceType

                -- Disable piece select if all are 0
                pieceSelectDisabled =
                    updatedInventory.l == 0 && updatedInventory.t == 0 && updatedInventory.z == 0 && updatedInventory.o == 0

                cmd =
                    if showInvalid then
                        Process.sleep (2 * 1000)
                            |> Task.perform (\_ -> HideInvalid)
                    else
                        Cmd.none

                sendInvalidLoad =
                    if (not showInvalid) && (not (case newGameState of
                                                    Just gameStateVal -> gameStateVal.gameOver
                                                    Nothing -> False
                                                ))
                    then
                        [ Task.succeed InvalidLoad |> Task.perform identity ]
                    else
                        []
            in
            ( { model
                | gameState = newGameState
                , showInvalid = showInvalid
                , inventory = updatedInventory
                , pieceType = nextPieceType
                , pieceSelectDisabled = pieceSelectDisabled
                , aiThinking = False
              }
            , Cmd.batch (cmd :: sendInvalidLoad)
            )

        HideInvalid ->
            ( { model | showInvalid = False }, Cmd.none )

        PlacePiece ->
            (model, submitMove model.pieceType model.pieceRotIndex model.pieceY model.pieceX)

        PlacePieceResult gamestate ->
            let
                showInvalid = gamestate.moveInvalid
                newGameState = Just gamestate
            in
            ( { model | gameState = newGameState, showInvalid = showInvalid }
            , Cmd.none
            )

        SelectPieceType ptype ->
            let
                pieceAvailable =
                    case ptype of
                        LShape -> model.inventory.l > 0
                        TShape -> model.inventory.t > 0
                        ZShape -> model.inventory.z > 0
                        OShape -> model.inventory.o > 0
            in
            if model.pieceSelectDisabled then
                (model, Cmd.none)
            else
                let
                    newRotIndex = 0
                    cubeOffsets = getCubeOffsets ptype newRotIndex
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
                if pieceAvailable then
                    ( { model
                        | pieceType = ptype
                        , pieceRotIndex = newRotIndex
                        , pieceX = newX
                        , pieceY = newY
                        , pieceZ = newZ
                      }
                    , Cmd.none
                    )
                else
                    (model, Cmd.none)

        RotatePieceKey keyStr -> updatePiceRotation keyStr model
