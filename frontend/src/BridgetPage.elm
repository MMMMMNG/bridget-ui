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

-- MODEL

type PieceType
    = LShape
    | TShape
    | ZShape
    | OShape


type Player
    = Player1
    | Player2

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

type Axis = X | Y | Z 



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
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

        PlacePiece ->
            let
                nextPlayer =
                    case model.currentPlayer of
                        Player1 -> Player2
                        Player2 -> Player1
            in
            ( { model | currentPlayer = nextPlayer }, Cmd.none )



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
        Ok "a"          -> RotatePiece Z 1
        Ok "d"          -> RotatePiece Z -1
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
        entities =
            chessboard ++ [ gamePiece model ] ++ compassEntities

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

        playerText =
            "Player: "
                ++ (case model.currentPlayer of
                        Player1 -> "1"
                        Player2 -> "2"
                   )
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
        [ Html.button
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "z-index" "10"
            , Html.Attributes.style "top" "20px"
            , Html.Attributes.style "left" "20px"
            , Html.Events.onClick SwitchPieceType
            ]
            [ Html.text "Switch Piece" ]
        , Html.button
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
            , Html.Attributes.style "top" "60px"
            , Html.Attributes.style "left" "20px"
            , Html.Attributes.style "background" "rgba(255,255,255,0.8)"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "border-radius" "6px"
            , Html.Attributes.style "font-family" "monospace"
            ]
            [ Html.div []
                [ Html.text playerText ]
            , Html.text ("Center: " ++ centerCoords ++ " | " ++ rotIndexText)
            ]
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
        ]

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
