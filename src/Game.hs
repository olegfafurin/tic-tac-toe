{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}


module Game
    ( moveCursor
    , newGame
    , Game
    , Name
    , Cell (..)
    , Turn (..)
    , CellState (..)
    , Direction (..)
    , selected, zeroes, crosses, size, fixedGame, changeTurn, turn, errGame, makeMove, starts, gid
    ) where

import Control.Lens hiding ((<|), (|>), (:>), (:<),(.=), Empty)
import Linear.V2 (V2(..), _x, _y)
-- import Control.Comonad
import System.Random
import Data.Sequence (Seq(..), (<|))
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Text (Text)
import Data.Sequence (Seq( (:<|) ) )
import Web.HttpApiData
import Brick (App (..), AttrMap, AttrName, Widget, Next, BrickEvent(..), EventM, defaultMain, padLeftRight, hBox, vBox, withAttr, neverShowCursor, str, attrMap, on, continue, halt)


type Cell = V2 Int
data Game = Game
    { _gid :: Maybe Int
    , _turn :: Turn
    , _starts :: Turn
    , _randomSeed :: Int
    , _movesDone :: Int
    , _size :: Int
    , _err :: Bool
    , _game_over :: Bool
    , _field :: [Cell]
    , _crosses :: Seq Cell
    , _zeroes :: Seq Cell
    , _selected :: Cell
    }
    deriving (Show, Eq, Generic)

data Turn = Player | Computer
    deriving (Show, Eq, Generic)

data CellState = EmptyCell | Cross | Zero | Selected
    deriving (Show, Eq)

data Direction = UpDir | RightDir | DownDir | LeftDir
    deriving (Show)


instance ToJSON (V2 Int) where
    toJSON (V2 x y) = object ["x" .= x, "y" .= y]

    toEncoding (V2 x y) = pairs ("x" .= x <> "y" .= y)

instance FromJSON (V2 Int) where
    parseJSON = withObject "V2 Int" $ \v -> V2
        <$> v .: "x"
        <*> v .: "y"

-- instance FromHttpApiData (V2 Int)
-- instance ToHttpApiData (V2 Int)

instance ToJSON Turn
instance ToJSON Game
instance FromJSON Turn
instance FromJSON Game

-- instance FromHttpApiData (V2 Int)
instance ToHttpApiData Turn where
    toUrlPiece :: Turn -> Text
    toUrlPiece Player = "player"
    toUrlPiece Computer = "computer"

instance FromHttpApiData Turn where
    parseUrlPiece :: Text -> Either Text Turn
    parseUrlPiece "player" = Right Player
    parseUrlPiece "computer" = Right Computer
    parseUrlPiece a = Left a

instance ToHttpApiData Cell where
    toUrlPiece :: (V2 Int) -> Text
    toUrlPiece (V2 x y) = T.pack $ show x ++ "_" ++ show y

instance FromHttpApiData Cell where
    parseUrlPiece :: Text -> Either Text (V2 Int)
    parseUrlPiece str = if '_' `elem` T.unpack str
                        then Right $ V2 (read . T.unpack $ a !! 0) (read . T.unpack $ a !! 1)
                        else Left str
                        where a = T.splitOn (T.pack "_") str


type Name = ()

makeLenses ''Game


oppositeSign :: CellState -> CellState
oppositeSign Cross = Zero
oppositeSign Zero = Cross
oppositeSign _ = error "no opposite sign"


moveCursor :: Direction -> Game -> Game
moveCursor dir game = case dir of
                UpDir -> if game^.selected._y < game^.size - 1
                            then game & selected._y +~ 1
                            else game
                RightDir -> if game^.selected._x < game^.size - 1
                            then game & selected._x +~ 1
                            else game
                DownDir -> if game^.selected._y > 0
                            then game & selected._y -~ 1
                            else game
                LeftDir -> if game^.selected._x > 0
                            then game & selected._x -~ 1
                            else game



fixedGame :: Game
fixedGame = Game { _gid = Nothing
                 , _turn = Player
                 , _starts = Player
                 , _movesDone = 0
                 , _err = False
                 , _randomSeed = 1
                 , _game_over = False
                 , _size = 10
                 , _field = [V2 x y | y <- [0..9], x <- [0..9]]
                 , _crosses = Empty
                 , _zeroes = Empty
                 , _selected = V2 5 5
                 }


changeTurn :: Turn -> Turn
changeTurn Computer = Player
changeTurn Player = Computer

errGame :: Game
errGame = newGame 101 0 1 Computer

newGame :: Int -> Int -> Int -> Turn -> Game
newGame gameId boardSize seed turn = Game { _gid = Just gameId
                              , _turn = turn
                              , _starts = turn
                              , _err = False
                              , _randomSeed = seed
                              , _game_over = False
                              , _movesDone = 0
                              , _size = boardSize
                              , _field = [V2 x y | y <- [0..boardSize - 1], x <- [0..boardSize - 1]]
                              , _crosses = if turn == Computer && gameId /= (-1) then (head $ genCell $ randomRs (0, boardSize - 1) (mkStdGen seed)) :<| Empty else Empty
                              , _zeroes = Empty
                              , _selected = V2 (boardSize `div` 2) (boardSize `div` 2)
                              }

genCell :: [Int] -> [Cell]
genCell (a : b : rest) = V2 a b : genCell rest
genCell [x] = error "function genCell is designed for infinite lists only"
genCell [] = []


getRandomCell :: Int -> IO Cell
getRandomCell a = do
    x <- getStdRandom $ randomR (0, a)
    y <- getStdRandom $ randomR (0, a)
    return $ V2 x y


makeMove :: CellState -> Game -> IO Game
makeMove sym game = do
    let gen = mkStdGen $ game ^. randomSeed
    let newCell = ((take 1 $ filter ((&&) <$> (flip notElem (game ^. crosses)) <*> (flip notElem (game ^. zeroes))) $ genCell $ randomRs (0, game^.size - 1) gen) !! 0)
    let signField = if (sym == Cross) then crosses else zeroes
    newSeed <- randomRIO (0, game^.size - 1)
    return $ game & signField %~ ((:<|) newCell) & randomSeed .~ newSeed
