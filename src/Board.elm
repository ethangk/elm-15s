module Board exposing (..)

import Browser
import Task
import Random.List exposing(shuffle)
import Random exposing (Seed, generate)
import List.Split exposing(chunksOfLeft)
import Time
import Html.Events exposing (onClick)
import Html exposing (Attribute, div, text, li, ul)
import Html.Attributes exposing(style)
import Html.Events exposing (on)
import List.Extra exposing(elemIndex, swapAt)
import String.Interpolate exposing(interpolate)

-- MAIN

boardWidth : Int
boardWidth = 4
boardHeight : Int
boardHeight = 4

modelStart : Model
modelStart =
    { squares = generatePieces
     ,moves = []
    }


main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( modelStart, generate ShufflePieces (shuffle modelStart.squares) )
    , view = view 
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL
type alias BoardWidth = Int
type alias BoardHeight = Int

type Move = Move Square Position

type alias Board = { squares: List Square, moves: List Move }

-- I want to build a board of width * height
-- I then need to create a Piece type, plus an EmptyPiece type

type alias Position = Int

type alias Value = Int
type Square = Piece Value | EmptyPiece


indexToPosition : Int -> Position
indexToPosition index = index + 1

positionToIndex : Position -> Int
positionToIndex position = position - 1

squareInCorrectPosition : Int -> Square -> Bool
squareInCorrectPosition index square =
  let
    position = index |> indexToPosition
  in
  case square of
     -- This feels kind of hacky
     Piece value -> value == position
     EmptyPiece -> False

squaresInCorrectPosition : List Square -> List Bool
squaresInCorrectPosition squares =
  squares |> List.indexedMap(squareInCorrectPosition)

type alias Model = Board

generatePieces : List Square
generatePieces =
  let pieces = List.range 1 ((boardWidth * boardHeight) - 1)
              |> List.map(\v -> Piece v)
  in
    EmptyPiece :: pieces


-- UPDATE


type Msg
  = ShufflePieces (List Square)
   | MoveSquare Square

moveSquare : Square -> List Square -> List Square
moveSquare square squares =
  let
    squareHasAdjacent = hasAdjacentEmptySquare square squares

  in
  case squareHasAdjacent of
    False -> squares
    True -> moveSquareInner square squares

moveSquareInner : Square -> List Square -> List Square
moveSquareInner square squares =
  let
    squarePosition = getSquarePosition square squares
    emptyPosition = getSquarePosition EmptyPiece squares
  in
  case (squarePosition, emptyPosition) of
    (Nothing, Nothing) -> squares
    (_, Nothing) -> squares
    (Nothing, _) -> squares
    (Just squarePos, Just emptyPos) -> swapAt (positionToIndex squarePos) (positionToIndex emptyPos) squares


isAdjacent : Position -> Position -> Bool
isAdjacent squarePos emptyPos =
  [
     squarePos - 1
    ,squarePos + 1
    ,squarePos + boardWidth
    ,squarePos - boardWidth
  ] |> List.any(\v -> v == emptyPos)

getSquarePosition : Square -> List Square -> Maybe Position
getSquarePosition square squares =
  let squareIndex = elemIndex square squares
  in
  case squareIndex of
    Just index -> Just (index |> indexToPosition)
    Nothing -> Nothing

hasAdjacentEmptySquare : Square -> List Square -> Bool
hasAdjacentEmptySquare square squares =
  let
    squarePosition = getSquarePosition square squares
    emptyPosition = getSquarePosition EmptyPiece squares
  in
  case (squarePosition, emptyPosition) of
    (Nothing, Nothing) -> False
    (_, Nothing) -> False
    (Nothing, _) -> False
    (Just squarePos, Just emptyPos) -> isAdjacent squarePos emptyPos

updateMoves : Square -> List Square -> List Move -> List Move
updateMoves square squares moves =
  let
    canMove = hasAdjacentEmptySquare square squares
    emptyPosition = getSquarePosition EmptyPiece squares
  in
  case (canMove, emptyPosition) of
    (True, Just pos) -> moves ++ [Move square pos]
    _ -> moves



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShufflePieces pieces ->
      ({ model | squares = pieces}, Cmd.none)
    MoveSquare square ->
      (
        {
          model | squares = moveSquare square model.squares
          , moves = updateMoves square model.squares model.moves
        }, Cmd.none
      )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

squareBackgroundColour : Bool -> String
squareBackgroundColour correctPosition =
  case correctPosition of
    True -> "teal"
    False -> "white"

displaySquare : (Square, Bool) -> Html.Html Msg
displaySquare (square, correctPosition) =
  Html.div [
     style "height" "50px"
    ,style "width" "50px"
    ,style "color" "black"
    ,style "font-size" "35px"
    ,style "vertical-align" "middle"
    ,style "display" "table-cell"
    ,style "border" "1px solid black"
    ,style "background-color" (squareBackgroundColour correctPosition)
    ,onClick (MoveSquare square)
  ] [text (getSquareContents square)]

getSquareContents : Square -> String
getSquareContents square =
  case square of
    EmptyPiece -> ""
    Piece value -> String.fromInt value

displayRow : List (Square, Bool) -> Html.Html Msg
displayRow row =
  div [] <| (row |> List.map(displaySquare))

generateSquaresWithBool : List Square -> List (Square, Bool)
generateSquaresWithBool squares =
  List.map2 Tuple.pair squares (squaresInCorrectPosition squares)

-- I need to work out what ones are 'adjacent' to the blank square
-- That's going to be all the values that are 

getMoveWeight : Move -> String
getMoveWeight (Move square position) =
  case square of
    Piece value ->
      case value == position of
        True -> "bold"
        _ -> "normal"
    EmptyPiece -> "normal"
      

displayMove : Move -> Html.Html Msg
displayMove (Move square position) =
  let move = Move square position
  in
  case square of
    Piece squareValue -> li [style "font-weight" (getMoveWeight move)] [text (interpolate "{0} -> {1}" [squareValue |> String.fromInt, position |> String.fromInt])]
    EmptyPiece -> li [] []

displayMoves : List Move -> Html.Html Msg
displayMoves moves =
  ul [] <| (moves |> List.map(displayMove))

view : Model -> Html.Html Msg
view model =
  let
    squaresWithBool = generateSquaresWithBool model.squares
    rows = chunksOfLeft boardWidth squaresWithBool
  in
    div [] [
       div [] <| (rows |> List.map(displayRow))
      ,div [] [model.moves |> displayMoves]
    ]
