{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
  ( moveCursor
  , newGame
  , Game (..)
  , Name
  , Cell(..)
  , Player(..)
  , Display(..)
  , Direction(..)
  , selected
  , zeroes
  , crosses
  , size
  , starts
  , gid
  , game
  , winner
  , oppositePlayer
  , makeMove
  , isGameOver
  , getWinner
  , getFirstPlayer
  ) where

import Control.Lens hiding ((:<), (:>), Empty, (.=), (<|), (|>))
import Data.Aeson
import Data.List
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Random
import Web.HttpApiData

data Cell = Cell
  { __x :: Int
  , __y ::Int
  }
  deriving (Show, Eq, Generic)

data Game =
  Game
    { _gid :: Int
    , _starts :: Player
    , _size :: Int
    , _winner :: Maybe Player
    , _crosses :: [Cell]
    , _zeroes :: [Cell]
    }
  deriving (Show, Eq, Generic)

data Player
  = User
  | Computer
  deriving (Show, Eq, Generic)

data Display =
  Display
    { _game :: Game
    , _selected :: Cell
    }

data Direction
  = UpDir
  | RightDir
  | DownDir
  | LeftDir
  deriving (Show)

instance ToJSON Cell where
  toJSON (Cell x y) = object ["x" .= x, "y" .= y]
  toEncoding (Cell x y) = pairs ("x" .= x <> "y" .= y)

instance FromJSON Cell where
  parseJSON = withObject "Cell" $ \v -> Cell <$> v .: "x" <*> v .: "y"

instance ToJSON Player

instance ToJSON Game

instance FromJSON Player

instance FromJSON Game

instance ToHttpApiData Player where
  toUrlPiece :: Player -> Text
  toUrlPiece User = "player"
  toUrlPiece Computer = "computer"

instance FromHttpApiData Player where
  parseUrlPiece :: Text -> Either Text Player
  parseUrlPiece "player" = Right User
  parseUrlPiece "computer" = Right Computer
  parseUrlPiece a = Left a

instance ToHttpApiData Cell where
  toUrlPiece :: Cell -> Text
  toUrlPiece (Cell x y) = T.pack $ show x ++ "_" ++ show y

instance FromHttpApiData Cell where
  parseUrlPiece :: Text -> Either Text Cell
  parseUrlPiece str =
    if '_' `elem` T.unpack str
      then Right $ Cell (read . T.unpack $ a !! 0) (read . T.unpack $ a !! 1)
      else Left str
    where
      a = T.splitOn (T.pack "_") str

type Name = ()

makeLenses ''Game

makeLenses ''Display

makeLenses ''Cell

moveCursor :: Direction -> Display -> Display
moveCursor dir d = d & selected .~ newSelection
  where
    n = d ^. game . size
    sel :: Cell = d ^. selected
    newSelection :: Cell =
      case dir of
        UpDir ->
          if sel ^. _y < n - 1
            then sel & _y +~ 1
            else sel
        RightDir ->
          if sel ^. _x < n - 1
            then sel & _x +~ 1
            else sel
        DownDir ->
          if sel ^. _y > 0
            then sel & _y -~ 1
            else sel
        LeftDir ->
          if sel ^. _x > 0
            then sel & _x -~ 1
            else sel

oppositePlayer :: Player -> Player
oppositePlayer Computer = User
oppositePlayer User = Computer

newGame :: Int -> Int -> Player -> Game
newGame gameId boardSize turn =
  Game
    { _gid = gameId
    , _starts = turn
    , _size = boardSize
    , _winner = Nothing
    , _crosses =
        if turn == Computer && gameId /= (-1)
          then [(head $ genCell $ randomRs (0, boardSize - 1) (mkStdGen 241))]
          else []
    , _zeroes = []
    }

genCell :: [Int] -> [Cell]
genCell (a:b:rest) = Cell a b : genCell rest
genCell [_] = error "function genCell is designed for infinite lists only"
genCell [] = []

getFirstPlayer :: IO Player
getFirstPlayer = do
  randomTurn <- (randomIO :: IO Bool)
  return $
    case randomTurn of
      True -> Computer
      False -> User

winStrike :: Int -> Int
winStrike boardSize
  | boardSize == 3 = 3
  | boardSize <= 5 = 4
  | otherwise = 5

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf l x = intersect l x == l

getWinner :: Game -> Maybe Player
getWinner g
  | g ^. winner /= Nothing = g ^. winner
  | otherwise =
    if (any (flip isSubsetOf (g ^. crosses)) (possibleWinSets g))
      then Just $ g ^. starts
      else if (any (flip isSubsetOf (g ^. zeroes)) (possibleWinSets g))
             then Just $ oppositePlayer $ g ^. starts
             else Nothing

possibleWinSets :: Game -> [[Cell]]
possibleWinSets g = concat $ rows <> columns <> mainDiag <> sideDiag
  where
    rows =
      [ [[Cell i (j + k) | k <- [0 .. w - 1]] | j <- [0 .. s - w]] | i <- [0 .. s - 1]]
    columns =
      [ [[Cell (i + k) j | k <- [0 .. w - 1]] | i <- [0 .. s - w]] | j <- [0 .. s - 1]]
    mainDiag =
      [ [[Cell (i + k) (j + k) | k <- [0 .. w - 1]] | i <- [0 .. s - w]] | j <- [0 .. s - w]]
    sideDiag =
      [ [[Cell (i + k) (j - k) | k <- [0 .. w - 1]] | i <- [0 .. s - w]] | j <- [w - 1 .. s - 1]]
    w = winStrike s
    s = g ^. size

isGameOver :: Game -> Bool
isGameOver g
  | g ^. winner /= Nothing = True
  | (length $ g ^. crosses <> g ^. zeroes) == (g ^. size) ^ (2 :: Int) = True
  | otherwise =
    case getWinner g of
      Just _ -> True
      Nothing -> False

movePriority :: ([Cell], [Cell]) -> [Cell] -> Int
movePriority (compSigns, userSigns) proposal =
  if free > 0
    then max 0 metric
    else metric
  where
    self = length $ proposal `intersect` compSigns
    enemy = length $ proposal `intersect` userSigns
    win = length proposal
    free = win - self - enemy
    metric = self - enemy * win

makeMove :: Game -> Game
makeMove g = proposal
  where
    alreadySetCells = (g ^. compLens, g ^. userLens)
    pos = possibleWinSets g
    available = maximumBy (comparing $ movePriority $ alreadySetCells) pos
    compSigns = fst alreadySetCells
    userSigns = snd alreadySetCells
    (compLens, userLens) =
      case g ^. starts of
        User -> (zeroes, crosses)
        Computer -> (crosses, zeroes)
    findFreeCell :: [Cell] -> Maybe Cell =
      find (flip notElem $ compSigns <> userSigns)
    cl =
      case g ^. starts of
        User -> zeroes
        Computer -> crosses
    proposal =
      case findFreeCell available of
        Just cell -> g & cl %~ (<> [cell])
        Nothing -> g
