{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Client (main) where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
-- import Servant.API
import Servant.Client
import Servant
-- import Servant.Types.SourceT (foreach)
import Game (newGame, Turn (..))
import UI (handleEvent, gameAttrMap, drawUI)
import API (UserAPI, userAPI)
import Brick (App (..), AttrMap, AttrName, Widget, Next, BrickEvent(..), EventM, defaultMain, padLeftRight, hBox, vBox, withAttr, neverShowCursor, str, attrMap, on, continue, halt)
import System.Random


main :: IO ()
main = do
    let app = App drawUI neverShowCursor handleEvent return (const gameAttrMap)
    putStrLn "Enter grid size (side of the square):"
    side :: Int <- readLn
    randomSeed <- (randomIO :: IO Int)
    randomTurn <- (randomIO :: IO Bool)
    let firstPlayer = case randomTurn of
            True  -> Computer
            False -> Player
    let pseudoGame = newGame (-1) side randomSeed firstPlayer
    let sendNewGame :<|> sendMove = client userAPI
    finalState <- defaultMain app pseudoGame
    putStrLn $ show pseudoGame
    putStrLn "Goodbye!"
