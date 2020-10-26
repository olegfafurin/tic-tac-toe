{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module UI
  ( gameAttrMap
  , drawUI
  ) where

import API
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens hiding ((:<), (:>), Empty, (<|), (|>))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Game
import qualified Graphics.Vty as V
import Linear.V2 (V2(..), _x, _y)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import Servant.Client
-- import Servant.Types.SourceT (foreach)



drawUI :: Game -> [Widget Name]
drawUI g = [C.center $ drawGrid g]

drawGrid :: Game -> Widget Name
drawGrid g = B.borderWithLabel (str "tic-tac-toe") $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [g ^. size - 1,g ^. size - 2 .. 0]]
    cellsInRow y =
      [padLeftRight 1 $ drawCoord (V2 x y) | x <- [0 .. g ^. size - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c == g ^. selected = Selected
      | c `elem` g ^. crosses = Cross
      | c `elem` g ^. zeroes = Zero
      | otherwise = EmptyCell

drawCell :: CellState -> Widget Name
drawCell state =
  case state of
    EmptyCell -> withAttr emptyAttr (str ".")
    Cross -> withAttr crossAttr (str "X")
    Zero -> withAttr zeroAttr (str "O")
    Selected -> withAttr selectedAttr (str " ")

emptyAttr, crossAttr, zeroAttr, selectedAttr :: AttrName
crossAttr = "cross"

zeroAttr = "zero"

selectedAttr = "selected"

emptyAttr = "empty"

gameAttrMap =
  attrMap
    V.defAttr
    [ (crossAttr, fg V.red)
    , (zeroAttr, fg V.blue)
    , (selectedAttr, V.black `on` V.yellow)
    ]

-- main :: IO ()
-- main = do
--   let app =
--         App
--           { appDraw = drawUI
--           , appChooseCursor = neverShowCursor
--           , appHandleEvent = handleEvent
--           , appStartEvent = return
--           , appAttrMap = const gameAttrMap
--           }
--   putStrLn "Enter random seed:"
--   (sd :: Int) <- readLn
--   let initialState = newGame 0 10 sd User
--   putStrLn $ show initialState
--   finalState <- defaultMain app initialState
--   putStrLn "Done"
