module BridgetPage exposing (main)

import Angle
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events
import Camera3d
import Color
import Direction3d
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Task
import Viewpoint3d
import Html.Events
import Rotations
import Svg
import Svg.Attributes

import Json.Decode as D
import Json.Decode exposing (Decoder)
import Url.Builder
import Http
-- MODEL

type PieceType
    = LShape
    | TShape
    | ZShape
    | OShape


type Player
    = Player1
    | Player2

type alias Inventory =
    { l : Int
    , z : Int
    , t : Int
    , o : Int
    }

type alias Model =
    { azimuth : Float
    , elevation : Float
    , isMouseDown : Bool
    , lastMousePos : Maybe (Float, Float)
    , windowWidth : Float
    , windowHeight : Float
    , pieceRotIndex : Int
    , shiftKeyPressed : Bool
    , pieceX : Int
    , pieceY : Int
    , pieceZ : Int
    , pieceType : PieceType
    , currentPlayer : Player
    , gameState : Maybe GameState
    , showInvalid : Bool
    , inventory : Inventory
    }

type Msg
    = MouseDown Float Float
    | MouseUp
    | MouseMove Float Float
    | NoOp
    | WindowResize Float Float
    | KeyChanged Bool
    | MovePiece Int Int
    | RotatePiece Axis Int
    | SwitchPieceType
    | PlacePiece
    | PlacePieceResult GameState
    | SelectPieceType PieceType
    | PlacePieceHttpResult (Result Http.Error (Maybe GameState))

type Axis = X | Y | Z 

type alias GameState =
    { gameOver    : Bool
    , moveInvalid : Bool
    , winner      : String
    , board       : List (List (List Int))
    }

-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    let
        testGameState : GameState
        testGameState =
            { gameOver = False
            , moveInvalid = False
            , winner = "Player 1"
            , board =
                [ [ [ 0, 0, 0 ], [ 0, 2, 0 ], [ 0, 0, 0 ] ]
                , [ [ 0, 1, 0 ], [ 0, 2, 0 ], [ 0, 0, 0 ] ]
                , [ [ 0, 0, 0 ], [ 0, 0, 0 ], [ 0, 0, 0 ] ]
                ]
            }
        showInvalidInit =
            if testGameState.moveInvalid then
                True
            else
                False
        initialInventory =
            { l = 4, z = 4, t = 4, o = 2 }
    in
    ( { azimuth = 45
      , elevation = 30
      , isMouseDown = False
      , lastMousePos = Nothing
      , windowWidth = 0
      , windowHeight = 0
      , shiftKeyPressed = False
      , pieceRotIndex = 0
      , pieceX = 3
      , pieceY = 3
      , pieceZ = 0
      , pieceType = LShape
      , currentPlayer = Player1
      , gameState = Just testGameState
      , showInvalid = showInvalidInit
      , inventory = initialInventory
      }
    , Task.perform
        (\vp -> WindowResize vp.scene.width vp.scene.height)
        Dom.getViewport
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                cubeOffsets = getCubeOffsets model.pieceType model.pieceRotIndex
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

                newX = clamp minX maxX (model.pieceX + dx)
                newY = clamp minY maxY (model.pieceY + dy)
                newZ = clamp minZ maxZ model.pieceZ
            in
            ( { model | pieceX = newX, pieceY = newY, pieceZ = newZ }, Cmd.none )

        RotatePiece axis dir ->
            let
                (rotations, maxIndex) =
                    case model.pieceType of
                        LShape -> (Rotations.lBlockRotations, 24)
                        TShape -> (Rotations.tBlockRotations, 12)
                        ZShape -> (Rotations.zBlockRotations, 12)
                        OShape -> (Rotations.oBlockRotations, 2)
                newIndex =
                    let
                        idx = model.pieceRotIndex + dir
                    in
                    if idx < 0 then
                        maxIndex - 1
                    else if idx >= maxIndex then
                        0
                    else
                        idx

                cubeOffsets = getCubeOffsets model.pieceType newIndex
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
                | pieceRotIndex = newIndex
                , pieceX = newX
                , pieceY = newY
                , pieceZ = newZ
              }
            , Cmd.none
            )

        SwitchPieceType ->
            let
                newType =
                    case model.pieceType of
                        LShape -> TShape
                        TShape -> ZShape
                        ZShape -> OShape
                        OShape -> LShape

                newRotIndex = 0
                cubeOffsets = getCubeOffsets newType newRotIndex
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
                | pieceType = newType
                , pieceRotIndex = newRotIndex
                , pieceX = newX
                , pieceY = newY
                , pieceZ = newZ
              }
            , Cmd.none
            )
        
        PlacePieceHttpResult (Err _) -> (model, Cmd.none)
        PlacePieceHttpResult (Ok Nothing) -> (model, Cmd.none)
        PlacePieceHttpResult (Ok (Just gs)) -> update (PlacePieceResult gs) model

        PlacePiece ->
            -- ...your logic for sending the move to the backend...
            ( model, Cmd.none )

        PlacePieceResult gamestate ->
            let
                showInvalid = gamestate.moveInvalid
                newGameState = Just gamestate
            in
            ( { model | gameState = newGameState, showInvalid = showInvalid }, Cmd.none )

        SelectPieceType ptype ->
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
            ( { model
                | pieceType = ptype
                , pieceRotIndex = newRotIndex
                , pieceX = newX
                , pieceY = newY
                , pieceZ = newZ
              }
            , Cmd.none
            )
            ( { model | currentPlayer = nextPlayer }, Cmd.none )
        PlacePieceResult gamestate -> 
            (model, submitMove model.pieceType model.pieceRotIndex model.pieceX model.pieceY)



-- VIEW


boardSize : Int
boardSize = 8

boardHeight : Int
boardHeight = 3

squareSize : Float
squareSize =
    1

chessboard : List (Scene3d.Entity ())
chessboard =
    List.concatMap
        (\y ->
            List.map
                (\x ->
                    let
                        isOrigin = x == 0 && y == 0
                        isWhite = modBy 2 (x + y) == 0

                        color =
                            if isOrigin then
                                Color.red
                            else if isWhite then
                                Color.rgb255 232 206 235 -- light purple
                            else
                                Color.darkGray

                        centerX = toFloat x - (toFloat boardSize - 1) / 2
                        centerY = toFloat y - (toFloat boardSize - 1) / 2
                        centerZ = 0 - (toFloat boardHeight - 1) / 2 - 0.5
                    in
                    Block3d.centeredOn
                        (Frame3d.atPoint (Point3d.meters centerX centerY centerZ))
                        ( Length.meters squareSize
                        , Length.meters squareSize
                        , Length.meters 0.1
                        )
                        |> Scene3d.blockWithShadow (Material.matte color)
                )
                (List.range 0 (boardSize - 1))
        )
        (List.range 0 (boardSize - 1))

compassEntities : List (Scene3d.Entity ())
compassEntities =
    let
        labelSize = ( Length.meters 0.8, Length.meters 0.8, Length.meters 0.05 )
        z = 0 - (toFloat boardHeight - 1) / 2 - 0.5
        n = Block3d.centeredOn (Frame3d.atPoint (Point3d.meters 0 -4.5 z)) labelSize
                |> Scene3d.blockWithShadow (Material.matte (Color.rgb255 200 0 0))
        s = Block3d.centeredOn (Frame3d.atPoint (Point3d.meters 0 4.5 z)) labelSize
                |> Scene3d.blockWithShadow (Material.matte (Color.rgb255 0 120 0))
        w = Block3d.centeredOn (Frame3d.atPoint (Point3d.meters -4.5 0 z)) labelSize
                |> Scene3d.blockWithShadow (Material.matte (Color.rgb255 0 0 200))
        e = Block3d.centeredOn (Frame3d.atPoint (Point3d.meters 4.5 0 z)) labelSize
                |> Scene3d.blockWithShadow (Material.matte (Color.rgb255 200 200 0))
    in
    [ n, e, s, w ]

-- HTTP

pTs : PieceType -> String
pTs pt = case pt of
    LShape -> "L"
    OShape -> "O"
    ZShape -> "Z"
    TShape -> "T"

submitMove : PieceType -> Int -> Int -> Int -> Cmd Msg
submitMove piece rotIndex x y = 
    let 
        command = String.join " " [pTs piece, 
                                   String.fromInt rotIndex, 
                                   String.fromInt x, 
                                   String.fromInt y]
        u = Url.Builder.crossOrigin "http://localhost:3000" [ "move", command ] []
    in Http.get
        { url = u
        , expect = gsExpecter
        }

gsExpecter = Http.expectJson PlacePieceHttpResult gsDecoder

stringDecoderWithDefault : a -> D.Decoder a -> D.Decoder a
stringDecoderWithDefault defaultValue decoder =
    D.string
        |> D.andThen (\str ->
            case D.decodeString decoder str of
                Ok val -> D.succeed val
                Err _ -> D.succeed defaultValue
        )


gsDecoder : D.Decoder (Maybe GameState)
gsDecoder =
    -- Attempt to decode a single GameState object.
    -- If the object is not found or has issues, the outer `optional` (map Just, map Nothing) handles it.
    D.oneOf
        [ D.map Just <|
            D.map4 GameState
                (D.field "isGameOver" D.bool) -- Corrected from D.string to D.bool based on GameState
                (D.field "isLastMoveInvalid" D.bool)
                (D.field "winner" D.string)
                (D.field "board" (stringDecoderWithDefault [[[1]]] (D.list (D.list (D.list D.int)))))
        , D.succeed Nothing -- If the field is missing or decoding fails, return Nothing
        ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseDown (mouseDecoder MouseDown)
        , Browser.Events.onMouseUp (Decode.succeed MouseUp)
        , Browser.Events.onResize (\w h -> WindowResize (toFloat w) (toFloat h))
        , Browser.Events.onKeyDown (Decode.map keyToMsg Decode.value)
        , Browser.Events.onKeyUp (Decode.map (keyCheck False) Decode.value)
        , if model.isMouseDown then
            Browser.Events.onMouseMove (mouseDecoder MouseMove)

          else
            Sub.none
        ]


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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

view : Model -> Html Msg
view model =
    let
        -- Render pieces from the backend board (GameState)
        boardEntities =
            case model.gameState of
                Just gs ->
                    let
                        -- Move BoardPos type alias to the top-level if needed, or just use records inline
                        flatten =
                            List.concatMap (\(z, plane) ->
                                List.concatMap (\(y, row) ->
                                    List.indexedMap (\x v -> { x = x, y = y, z = z, v = v }) row
                                ) (List.indexedMap Tuple.pair plane)
                            ) (List.indexedMap Tuple.pair gs.board)

                        pieceEntities =
                            List.filter (\pos -> pos.v /= 0) flatten
                                |> List.map (\pos ->
                                    let
                                        color =
                                            case pos.v of
                                                1 -> Color.black
                                                2 -> Color.white
                                                _ -> Color.gray
                                        cx = toFloat pos.x - (toFloat boardSize - 1) / 2
                                        cy = toFloat pos.y - (toFloat boardSize - 1) / 2
                                        cz = toFloat pos.z - (toFloat boardHeight - 1) / 2
                                    in
                                    Block3d.centeredOn
                                        (Frame3d.atPoint (Point3d.meters cx cy cz))
                                        ( Length.meters 1, Length.meters 1, Length.meters 1 )
                                        |> Scene3d.blockWithShadow (Material.matte color)
                                )
                    in
                    pieceEntities

                Nothing ->
                    []

        entities =
            chessboard ++ boardEntities ++ [ gamePiece model ] ++ compassEntities

        cubeOffsets = getCubeOffsets model.pieceType model.pieceRotIndex
        centerBlock =
            case cubeOffsets of
                first :: _ -> first
                [] -> { x = 0, y = 0, z = 0 }

        rotationIndexFromElm =
            let
                rotations =
                    case model.pieceType of
                        LShape -> Rotations.lBlockRotations
                        TShape -> Rotations.tBlockRotations
                        ZShape -> Rotations.zBlockRotations
                        OShape -> Rotations.oBlockRotations
            in
            case List.drop model.pieceRotIndex rotations |> List.head of
                Just rotation -> rotation.index
                Nothing -> model.pieceRotIndex

        centerCoords =
            "(" ++ String.fromInt (model.pieceX + centerBlock.x)
            ++ ", "
            ++ String.fromInt (model.pieceY + centerBlock.y)
            ++ ", "
            ++ String.fromInt (model.pieceZ + centerBlock.z)
            ++ ")"

        rotIndexText =
            "Rotation: " ++ String.fromInt rotationIndexFromElm

        pieceCount : PieceType -> Int
        pieceCount pt =
            case pt of
                LShape -> model.inventory.l
                ZShape -> model.inventory.z
                TShape -> model.inventory.t
                OShape -> model.inventory.o

        miniShape : PieceType -> (Float, Float) -> Html Msg
        miniShape pieceType (left, top) =
            let
                miniOffsets = getCubeOffsets pieceType 0
                color =
                    case pieceType of
                        OShape -> Color.yellow
                        TShape -> Color.red
                        ZShape -> Color.green
                        LShape -> Color.orange
                centerColor = Color.purple
                size = 16
                offset = 24
                -- Find min/max for normalization
                minX = List.minimum (List.map .x miniOffsets) |> Maybe.withDefault 0
                minY = List.minimum (List.map .y miniOffsets) |> Maybe.withDefault 0
                maxX = List.maximum (List.map .x miniOffsets) |> Maybe.withDefault 0
                maxY = List.maximum (List.map .y miniOffsets) |> Maybe.withDefault 0
                widthVal = maxX - minX + 1
                heightVal = maxY - minY + 1
                count = pieceCount pieceType
            in
            Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "left" (String.fromFloat left ++ "px")
                , Html.Attributes.style "top" (String.fromFloat top ++ "px")
                , Html.Attributes.style "width" (String.fromInt (widthVal * size + 2 * offset + 48) ++ "px")
                , Html.Attributes.style "height" (String.fromInt (heightVal * size + 2 * offset + 24) ++ "px")
                , Html.Attributes.style "z-index" "20"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "align-items" "center"
                ]
                [ Html.div
                    [ Html.Attributes.style "font-size" "28px"
                    , Html.Attributes.style "font-weight" "bold"
                    , Html.Attributes.style "color" "#333"
                    , Html.Attributes.style "margin-right" "12px"
                    , Html.Attributes.style "width" "40px"
                    , Html.Attributes.style "text-align" "right"
                    ]
                    [ Html.text ("x " ++ String.fromInt count) ]
                , Html.map (\_ -> SelectPieceType pieceType)
                    (Svg.svg
                        [ Svg.Attributes.width (String.fromInt (widthVal * size + 2 * offset))
                        , Svg.Attributes.height (String.fromInt (heightVal * size + 2 * offset))
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Events.onClick (SelectPieceType pieceType)
                        ]
                        (List.indexedMap
                            (\i p ->
                                let
                                    xVal = (p.x - minX) * size + offset
                                    yVal = (p.y - minY) * size + offset
                                    fillVal = if i == 0 then "purple" else
                                        case pieceType of
                                            OShape -> "yellow"
                                            TShape -> "red"
                                            ZShape -> "green"
                                            LShape -> "orange"
                                in
                                Svg.rect
                                    [ Svg.Attributes.x (String.fromInt xVal)
                                    , Svg.Attributes.y (String.fromInt yVal)
                                    , Svg.Attributes.width (String.fromInt size)
                                    , Svg.Attributes.height (String.fromInt size)
                                    , Svg.Attributes.fill fillVal
                                    , Svg.Attributes.stroke "#333"
                                    , Svg.Attributes.strokeWidth "2"
                                    , Svg.Attributes.rx "4"
                                    , Svg.Attributes.ry "4"
                                    ]
                                    []
                            )
                            miniOffsets
                        )
                    )
                ]

        -- List of shapes and their vertical positions
        shapeSvgs : List (Html Msg)
        shapeSvgs =
            [ (OShape, 120)
            , (ZShape, 180)
            , (LShape, 240)
            , (TShape, 300)
            ]
            |> List.map (\(ptype, top) -> miniShape ptype (20, toFloat top))

    in
    Html.div
        [ Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        , Html.Attributes.style "margin" "0"
        , Html.Attributes.style "padding" "0"
        , Html.Attributes.style "overflow" "hidden"
        , Html.Attributes.style "user-select" "none"
        ]
        (
            shapeSvgs
            ++
            (if model.showInvalid then
                [ Html.div
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "top" "0"
                    , Html.Attributes.style "left" "0"
                    , Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "background" "#ffcccc"
                    , Html.Attributes.style "color" "#a00"
                    , Html.Attributes.style "font-size" "20px"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "12px"
                    , Html.Attributes.style "z-index" "100"
                    ]
                    [ Html.text "Move invalid, try again!" ]
                ]
              else
                []
            )
            ++
            [
            Html.button
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "z-index" "10"
                , Html.Attributes.style "top" "50%"
                , Html.Attributes.style "right" "20px"
                , Html.Attributes.style "transform" "translateY(-50%)"
                , Html.Attributes.style "padding" "16px 24px"
                , Html.Attributes.style "font-size" "18px"
                , Html.Attributes.style "background" "#6c47a6"
                , Html.Attributes.style "color" "white"
                , Html.Attributes.style "border" "none"
                , Html.Attributes.style "border-radius" "8px"
                , Html.Attributes.style "box-shadow" "0 2px 8px rgba(0,0,0,0.15)"
                , Html.Events.onClick PlacePiece
                ]
                [ Html.text "Place" ]
            , Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "z-index" "10"
                , Html.Attributes.style "top" "20px"
                , Html.Attributes.style "left" "20px"
                , Html.Attributes.style "background" "rgba(255,255,255,0.8)"
                , Html.Attributes.style "padding" "8px"
                , Html.Attributes.style "border-radius" "6px"
                , Html.Attributes.style "font-family" "monospace"
                ]
                [ Html.text ("Center: " ++ centerCoords ++ " | " ++ rotIndexText)]
            , Scene3d.sunny
                { camera = createCamera model
                , entities = entities
                , background = Scene3d.backgroundColor (Color.rgb255 232 206 235)
                , clipDepth = Length.meters 0.1
                , dimensions =
                    ( Pixels.pixels (round model.windowWidth)
                    , Pixels.pixels (round model.windowHeight)
                    )
                , shadows = True
                , sunlightDirection = Direction3d.xyZ (Angle.degrees -120) (Angle.degrees -45)
                , upDirection = Direction3d.positiveZ
                }
                |> Html.map (\_ -> NoOp)
            , (case model.gameState of
                Just gs ->
                    if gs.gameOver then
                        Html.div
                            [ Html.Attributes.style "position" "fixed"
                            , Html.Attributes.style "top" "0"
                            , Html.Attributes.style "left" "0"
                            , Html.Attributes.style "width" "100vw"
                            , Html.Attributes.style "height" "100vh"
                            , Html.Attributes.style "background" "rgba(0,0,0,0.5)"
                            , Html.Attributes.style "display" "flex"
                            , Html.Attributes.style "align-items" "center"
                            , Html.Attributes.style "justify-content" "center"
                            , Html.Attributes.style "z-index" "200"
                            ]
                            [ Html.div
                                [ Html.Attributes.style "background" "white"
                                , Html.Attributes.style "padding" "40px"
                                , Html.Attributes.style "border-radius" "16px"
                                , Html.Attributes.style "font-size" "28px"
                                , Html.Attributes.style "color" "#222"
                                , Html.Attributes.style "box-shadow" "0 4px 32px rgba(0,0,0,0.25)"
                                ]
                                [ Html.text ("Game Over! Winner is: " ++ gs.winner) ]
                            ]
                    else
                        Html.text ""
                Nothing ->
                    Html.text ""
            )
            ]
        )

gamePiece : Model -> Scene3d.Entity ()
gamePiece model =
    let
        centerColor = Color.purple
        normalColor = case model.pieceType of
            OShape -> Color.yellow
            TShape -> Color.red
            ZShape -> Color.green
            LShape -> Color.orange

        cubeOffsets = getCubeOffsets model.pieceType model.pieceRotIndex
        cubeEntities =
            List.indexedMap
                (\i p ->
                    let
                        cx = toFloat (model.pieceX + p.x) - (toFloat boardSize - 1) / 2
                        cy = toFloat (model.pieceY + p.y) - (toFloat boardSize - 1) / 2
                        cz = toFloat (model.pieceZ + p.z) - (toFloat boardHeight - 1) / 2
                        color = if i == 0 then centerColor else normalColor
                    in
                    Block3d.centeredOn
                        (Frame3d.atPoint (Point3d.meters cx cy cz))
                        ( Length.meters 1, Length.meters 1, Length.meters 1 )
                        |> Scene3d.blockWithShadow (Material.matte color)
                )
                cubeOffsets
    in
    Scene3d.group cubeEntities

createCamera : Model -> Camera3d.Camera3d Length.Meters ()
createCamera model =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint =
                    Point3d.meters
                        (16 * cos (degrees model.elevation) * cos (degrees model.azimuth))
                        (16 * cos (degrees model.elevation) * sin (degrees model.azimuth))
                        (16 * sin (degrees model.elevation))
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 45
        }
