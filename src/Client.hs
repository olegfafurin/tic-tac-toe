{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Client
  ( main
  ) where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (defaultManagerSettings, newManager)

import Servant

-- import Servant.API
import Servant.Client

import API (UserAPI, userAPI)
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

-- import Servant.Types.SourceT (foreach)
import Game (Player(..), newGame)
import System.Random
import UI (drawUI, gameAttrMap, handleEvent, mkNewgame)

main :: IO ()
main = do
  let app = App drawUI neverShowCursor handleEvent return (const gameAttrMap)
  putStrLn "Enter grid size (side of the square):"
  side :: Int <- readLn
  randomSeed <- (randomIO :: IO Int)
  randomTurn <- (randomIO :: IO Bool)
  let firstPlayer =
        case randomTurn of
          True -> Computer
          False -> User
  pseudoGame <- mkNewgame side randomSeed firstPlayer
--     let sendNewGame :<|> sendMove = client userAPI
  finalState <- defaultMain app pseudoGame
  putStrLn $ show pseudoGame
  putStrLn "Goodbye!"
