{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}


module Game
    ( move
    , newGame
    , Game
    , Name
    , Cell (..)
    , Turn (..)
    , CellState (..)
    , Direction (..)
    , selected, zeroes, crosses, size, fixedGame, changeTurn, turn, errGame
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
import Web.HttpApiData
import Brick (App (..), AttrMap, AttrName, Widget, Next, BrickEvent(..), EventM, defaultMain, padLeftRight, hBox, vBox, withAttr, neverShowCursor, str, attrMap, on, continue, halt)


type Cell = V2 Int
data Game = Game
    { _id :: Maybe Int
    , _turn :: Turn
    , _starts :: Turn
    , _movesDone :: Int
    , _size :: Int
--     , _field :: Grid Cell
    , _field :: [Cell]
    , _crosses :: Seq Cell
    , _zeroes :: Seq Cell
    , _selected :: Cell
    }
    deriving (Show, Eq, Generic)

instance ToJSON (V2 Int) where
    toJSON (V2 x y) = object ["x" .= x, "y" .= y]

    toEncoding (V2 x y) = pairs ("x" .= x <> "y" .= y)

instance FromJSON (V2 Int) where
    parseJSON = withObject "V2 Int" $ \v -> V2
        <$> v .: "x"
        <*> v .: "y"

-- instance FromHttpApiData (V2 Int)
-- instance ToHttpApiData (V2 Int)

data Turn = Player | Computer
    deriving (Show, Eq, Generic)
data CellState = EmptyCell | Cross | Zero | Selected
    deriving (Show)
data Direction = UpDir | RightDir | DownDir | LeftDir
    deriving (Show)

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


move :: Direction -> Game -> Game
move dir game = case dir of
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
fixedGame = Game { _id = Nothing
                 , _turn = Player
                 , _starts = Player
                 , _movesDone = 0
                 , _size = 10
                 , _field = [V2 x y | y <- [0..9], x <- [0..9]]
                 , _crosses = Empty
                 , _zeroes = Empty
                 , _selected = V2 5 5
                 }

-- newGame :: Game
-- newGame = do
--     gameConfig <- initGameConfig
--     return gameConfig

changeTurn :: Turn -> Turn
changeTurn Computer = Player
changeTurn Player = Computer

errGame :: Game
errGame = newGame 0 Computer

newGame :: Int -> Turn -> Game
newGame boardSize turn = Game { _id = Nothing
                              , _turn = turn
                              , _starts = turn
                              , _movesDone = 0
                              , _size = boardSize
                              , _field = [V2 x y | y <- [0..boardSize - 1], x <- [0..boardSize - 1]]
                              , _crosses = Empty
                              , _zeroes = Empty
                              , _selected = V2 (boardSize `div` 2) (boardSize `div` 2)
                              }






