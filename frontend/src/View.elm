module View exposing (..)

import Html exposing (Html)
import Scene3d
import Types exposing (..)
import Direction3d
import Camera3d
import Html.Attributes
import Angle
import Pixels
import Viewpoint3d
import Html.Events
import Svg
import Svg.Attributes
import Color
import Block3d
import Frame3d
import Point3d
import Length
import Scene3d.Material as Material
import Constants exposing (boardSize, boardHeight, squareSize)
import Utils exposing (getCubeOffsets)
import Rotations

view : Model -> Html Msg
view model =
    let
        boardEntities =
            case model.gameState of
                Just gs ->
                    let
                        cubes =
                            List.concatMap (\(y, row) ->
                                List.concatMap (\(x, col) ->
                                    List.indexedMap (\z v ->
                                        if v == 0 then
                                            Nothing
                                        else
                                            let
                                                color =
                                                    case v of
                                                        1 -> Color.black
                                                        2 -> Color.white
                                                        3 -> Color.green
                                                        4 -> Color.red
                                                        _ -> Color.gray
                                                cubeEntity =
                                                    Block3d.centeredOn
                                                        (Frame3d.atPoint (Point3d.meters cx cy cz))
                                                        ( Length.meters 1, Length.meters 1, Length.meters 1 )
                                                        |> Scene3d.blockWithShadow (Material.matte color)
                                                cx = toFloat x - (toFloat boardSize - 1) / 2
                                                cy = toFloat y - (toFloat boardSize - 1) / 2
                                                cz = toFloat z - (toFloat boardHeight - 1) / 2
                                            in
                                            Just (cubeEntity)
                                    ) col
                                    |> List.filterMap identity
                                ) (List.indexedMap Tuple.pair row)
                            ) (List.indexedMap Tuple.pair gs.board)
                    in
                    cubes
                Nothing ->
                    []
        entities =
            chessboard ++ boardEntities ++ [ gamePiece model ] ++ compassEntities

        currentPieceCubeOffsets = getCubeOffsets model.pieceType model.pieceRotIndex
        centerBlock =
            case currentPieceCubeOffsets of
                first :: _ -> first
                [] -> { x = 0, y = 0, z = 0 }

        rotationIndexFromElm =
            let
                rotationGroups =
                    case model.pieceType of
                        LShape -> Rotations.lBlockRotationGroups
                        TShape -> Rotations.tBlockRotationGroups
                        ZShape -> Rotations.zBlockRotationGroups
                        OShape -> Rotations.oBlockRotationGroups
            in
            case List.drop model.pieceRotIndex rotationGroups |> List.head of
                Just group -> group.rotation.index
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
                minX = List.minimum (List.map .x miniOffsets) |> Maybe.withDefault 0
                minY = List.minimum (List.map .y miniOffsets) |> Maybe.withDefault 0
                maxX = List.maximum (List.map .x miniOffsets) |> Maybe.withDefault 0
                maxY = List.maximum (List.map .y miniOffsets) |> Maybe.withDefault 0
                widthVal = maxX - minX + 1
                heightVal = maxY - minY + 1
                count = pieceCount pieceType
                isDisabled = count == 0 || model.pieceSelectDisabled
            in
            Html.div
                ([ Html.Attributes.style "position" "absolute"
                 , Html.Attributes.style "left" (String.fromFloat left ++ "px")
                 , Html.Attributes.style "top" (String.fromFloat top ++ "px")
                 , Html.Attributes.style "width" (String.fromInt (widthVal * size + 2 * offset + 48) ++ "px")
                 , Html.Attributes.style "height" (String.fromInt (heightVal * size + 2 * offset + 24) ++ "px")
                 , Html.Attributes.style "z-index" "20"
                 , Html.Attributes.style "display" "flex"
                 , Html.Attributes.style "align-items" "center"
                 ]
                 ++ (if isDisabled then
                        [ Html.Attributes.style "opacity" "0.4"
                        , Html.Attributes.style "pointer-events" "none"
                        ]
                    else
                        []
                    )
                )
                [ Html.div
                    [ Html.Attributes.style "font-size" "28px"
                    , Html.Attributes.style "font-weight" "bold"
                    , Html.Attributes.style "color" "#333"
                    , Html.Attributes.style "margin-right" "12px"
                    , Html.Attributes.style "width" "40px"
                    , Html.Attributes.style "text-align" "right"
                    ]
                    [ Html.text ("x " ++ String.fromInt count) ]
                , Svg.svg
                    ([ Svg.Attributes.width (String.fromInt (widthVal * size + 2 * offset))
                     , Svg.Attributes.height (String.fromInt (heightVal * size + 2 * offset))
                     , Html.Attributes.style "cursor" (if isDisabled then "not-allowed" else "pointer")
                     , Html.Events.onClick (SelectPieceType pieceType)
                     ]
                     ++ (if isDisabled then [ Html.Attributes.disabled True ] else [])
                    )
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
            (if model.aiThinking then
                [ Html.div
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "top" "0"
                    , Html.Attributes.style "left" "0"
                    , Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "background" "#e0e0e0"
                    , Html.Attributes.style "color" "#333"
                    , Html.Attributes.style "font-size" "20px"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "12px"
                    , Html.Attributes.style "z-index" "101"
                    ]
                    [ Html.text "AI thinking..." ]
                ]
              else
                []
            )
            ++
            shapeSvgs
            ++
            (if model.showInvalid && not model.aiThinking then
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
                ([ Html.Attributes.style "position" "absolute"
                 , Html.Attributes.style "z-index" "10"
                 , Html.Attributes.style "top" "50%"
                 , Html.Attributes.style "right" "20px"
                 , Html.Attributes.style "transform" "translateY(-50%)"
                 , Html.Attributes.style "padding" "16px 24px"
                 , Html.Attributes.style "font-size" "18px"
                 , Html.Attributes.style "background" (if model.aiThinking then "#aaa" else "#6c47a6")
                 , Html.Attributes.style "color" "white"
                 , Html.Attributes.style "border" "none"
                 , Html.Attributes.style "border-radius" "8px"
                 , Html.Attributes.style "box-shadow" "0 2px 8px rgba(0,0,0,0.15)"
                 , Html.Events.onClick PlacePiece
                 ]
                 ++ (if model.aiThinking then [ Html.Attributes.disabled True ] else [])
                )
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