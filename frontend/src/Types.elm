module Types exposing (..)

import Http

-- TYPES
type PieceType
    = LShape
    | TShape
    | ZShape
    | OShape

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
    , gameState : Maybe GameState
    , showInvalid : Bool
    , inventory : Inventory
    , pieceSelectDisabled : Bool
    , aiThinking : Bool
    }

type Msg
    = MouseDown Float Float
    | MouseUp
    | MouseMove Float Float
    | NoOp
    | WindowResize Float Float
    | KeyChanged Bool
    | MovePiece Int Int
    | PlacePiece
    | PlacePieceResult GameState
    | SelectPieceType PieceType
    | PlacePieceHttpResult (Result Http.Error (Maybe GameState))
    | HideInvalid
    | InvalidLoad
    | RotatePieceKey String

type Axis = X | Y | Z 

type alias GameState =
    { gameOver    : Bool
    , moveInvalid : Bool
    , winner      : String
    , board       : List (List (List Int))
    }