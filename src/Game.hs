{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- {-# LANGUAGE LambdaCase #-}
module Game
  ( moveCursor
  , newGame
  , Game
  , Name
  , Cell(..)
  , Player(..)
  , CellState(..)
  , Direction(..)
  , selected
  , zeroes
  , crosses
  , size
  , fixedGame
  , oppositePlayer
  , errGame
  , makeMove
  , starts
  , gid
  , isGameOver
  ) where

import Control.Lens hiding ((:<), (:>), Empty, (.=), (<|), (|>))
import Linear.V2 (V2(..), _x, _y)

import Brick
  ( App(..)
  , AttrMap
  , AttrName
  , BrickEvent(..)
  , EventM
  , Next
  , Widget
  , attrMap
  , continue
  , defaultMain
  , hBox
  , halt
  , neverShowCursor
  , on
  , padLeftRight
  , str
  , vBox
  , withAttr
  )
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Random
import Web.HttpApiData

type Cell = V2 Int

data Game =
  Game
    { _gid :: Int
    , _starts :: Player
    , _size :: Int
    , _winner :: Maybe Player
    , _crosses :: [Cell]
    , _zeroes :: [Cell]
    , _selected :: Cell
    }
  deriving (Show, Eq, Generic)

data Player
  = User
  | Computer
  deriving (Show, Eq, Generic)

data CellState
  = EmptyCell
  | Cross
  | Zero
  | Selected
  deriving (Show, Eq)

data Direction
  = UpDir
  | RightDir
  | DownDir
  | LeftDir
  deriving (Show)

instance ToJSON (V2 Int) where
  toJSON (V2 x y) = object ["x" .= x, "y" .= y]
  toEncoding (V2 x y) = pairs ("x" .= x <> "y" .= y)

instance FromJSON (V2 Int) where
  parseJSON = withObject "V2 Int" $ \v -> V2 <$> v .: "x" <*> v .: "y"

-- instance FromHttpApiData (V2 Int)
-- instance ToHttpApiData (V2 Int)
instance ToJSON Player

instance ToJSON Game

instance FromJSON Player

instance FromJSON Game

-- instance FromHttpApiData (V2 Int)
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
  toUrlPiece :: (V2 Int) -> Text
  toUrlPiece (V2 x y) = T.pack $ show x ++ "_" ++ show y

instance FromHttpApiData Cell where
  parseUrlPiece :: Text -> Either Text (V2 Int)
  parseUrlPiece str =
    if '_' `elem` T.unpack str
      then Right $ V2 (read . T.unpack $ a !! 0) (read . T.unpack $ a !! 1)
      else Left str
    where
      a = T.splitOn (T.pack "_") str

type Name = ()

makeLenses ''Game

oppositeSign :: CellState -> CellState
oppositeSign Cross = Zero
oppositeSign Zero = Cross
oppositeSign _ = error "no opposite sign"

moveCursor :: Direction -> Game -> Game
moveCursor dir game =
  case dir of
    UpDir ->
      if game ^. selected . _y < game ^. size - 1
        then game & selected . _y +~ 1
        else game
    RightDir ->
      if game ^. selected . _x < game ^. size - 1
        then game & selected . _x +~ 1
        else game
    DownDir ->
      if game ^. selected . _y > 0
        then game & selected . _y -~ 1
        else game
    LeftDir ->
      if game ^. selected . _x > 0
        then game & selected . _x -~ 1
        else game

fixedGame :: Game
fixedGame =
  Game
    { _gid = (-1)
    , _starts = User
    , _size = 10
    , _winner = Nothing
    , _crosses = []
    , _zeroes = []
    , _selected = V2 5 5
    }

oppositePlayer :: Player -> Player
oppositePlayer Computer = User
oppositePlayer User = Computer

errGame :: Game
errGame = newGame 101 0 1 Computer

newGame :: Int -> Int -> Int -> Player -> Game
newGame gameId boardSize seed turn =
  Game
    { _gid = gameId
    , _starts = turn
    , _size = boardSize
    , _winner = Nothing
    , _crosses =
        if turn == Computer && gameId /= (-1)
          then [(head $ genCell $ randomRs (0, boardSize - 1) (mkStdGen seed))]
          else []
    , _zeroes = []
    , _selected = V2 (boardSize `div` 2) (boardSize `div` 2)
    }

genCell :: [Int] -> [Cell]
genCell (a:b:rest) = V2 a b : genCell rest
genCell [x] = error "function genCell is designed for infinite lists only"
genCell [] = []

getRandomCell :: Int -> IO Cell
getRandomCell a = do
  x <- getStdRandom $ randomR (0, a)
  y <- getStdRandom $ randomR (0, a)
  return $ V2 x y


winStrike :: Int -> Int
winStrike size
  | size == 3 = 3
  | size <= 5 = 4
  | otherwise = 5

getWinner :: Game -> Maybe Player
getWinner game =
  if (any (flip isInfixOf (game ^. crosses)) (possibleWinSets game))
    then Just $ game ^. starts
    else if (any (flip isInfixOf (game ^. zeroes)) (possibleWinSets game))
      then Just $ oppositePlayer $ game ^. starts
      else Nothing


possibleWinSets :: Game -> [[Cell]]
possibleWinSets game = concat $ rows <> columns <> mainDiag <> sideDiag
  where
  rows =
    [[[V2 i (j + k) | k <- [0 .. w - 1]] | j <- [0 .. s - w]] | i <- [0 .. s - 1]]
  columns =
    [[[V2 (i + k) j | k <- [0 .. w - 1]] | i <- [0 .. s - w]] | j <- [0 .. s - 1]]
  mainDiag =
    [[[V2 (i + k) (j + k) | k <- [0 .. w - 1]] | i <- [0 .. s - w]] | j <- [0 .. s - w]]
  sideDiag =
    [[[V2 (i + k) (j - k) | k <- [0 .. w - 1]] | i <- [0 .. s - w]] | j <- [w - 1 .. s - 1]]
  w = winStrike s
  s = game ^. size


isGameOver :: Game -> Bool
isGameOver game
  | (length $ game ^. crosses <> game ^. zeroes) == (game ^. size) ^ 2 = True
  | otherwise =
    case getWinner game of
      Just _ -> True
      Nothing -> False


movePriority :: ([Cell], [Cell]) -> [Cell] -> Int
movePriority (compSigns, userSigns) proposal =
  if (length $ proposal `intersect` userSigns) /= 0 then 0
  else if alreadySet == (length proposal) then 0 else alreadySet
  where alreadySet = length $ proposal `intersect` compSigns


makeMove :: Game -> Game
makeMove game = proposal
  where
    alreadySetCells = (game ^. compLens, game ^. userLens)
    pos = possibleWinSets game
    available :: [Cell] = maximumBy (comparing $ movePriority $ alreadySetCells) pos
    compSigns = fst alreadySetCells
    (compLens, userLens) = case game ^. starts of
      User     -> (zeroes, crosses)
      Computer -> (crosses, zeroes)
    findFreeCell :: [Cell] -> Maybe Cell = find (flip notElem compSigns)
    cl = case game ^. starts of
      User     -> zeroes
      Computer -> crosses
    proposal = case findFreeCell available of
      Just cell -> game & cl %~ (<> [cell])
      Nothing   -> game