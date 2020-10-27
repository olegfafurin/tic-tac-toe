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


drawUI :: Game -> [Widget Name]
drawUI g = [C.center $ (drawState g) <+> drawGrid g]
-- padRight (Pad 8)

drawState :: Game -> Widget Name
drawState g = hLimit 10
  $ vBox [ drawResult g
         , drawControls g]

drawControls :: Game -> Widget Name
drawControls g
    | isGameOver g =  str "press q or \n<ESC> to quit"
    | otherwise    = emptyWidget

drawResult :: Game -> Widget Name
drawResult g
    | isGameOver g = case g ^. winner of
        Just Computer -> withAttr loseAttr $ C.hCenter $ str "YOU LOSE"
        Just User     -> withAttr winAttr  $ C.hCenter $ str "YOU WON"
        Nothing       -> withAttr drawAttr  $ C.hCenter $ str "DRAW"
    | otherwise    = emptyWidget


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
winAttr = "winAttr"
loseAttr = "loseAttr"
drawAttr = "drawAttr"

gameAttrMap =
  attrMap
    V.defAttr
    [ (crossAttr, fg V.red)
    , (zeroAttr, fg V.blue)
    , (selectedAttr, V.black `on` V.yellow)
    , (loseAttr, fg V.red `V.withStyle` V.bold)
    , (winAttr, fg V.green `V.withStyle` V.bold)
    , (drawAttr, fg V.yellow `V.withStyle` V.bold)
    ]
