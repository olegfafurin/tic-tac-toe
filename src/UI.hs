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

drawUI :: Display -> [Widget Name]
drawUI d = [C.center $ (drawState $ d ^. game) <+> drawGrid d]

drawState :: Game -> Widget Name
drawState g = setAvailableSize (20, 40) $ vBox [drawResult g, drawControls g]

drawControls :: Game -> Widget Name
drawControls g
  | isGameOver g = txtWrap "press q or <ESC> to quit, r for restart"
  | otherwise = emptyWidget

drawResult :: Game -> Widget Name
drawResult g
  | isGameOver g =
    case g ^. winner of
      Just Computer -> withAttr loseAttr $ C.hCenter $ str "YOU LOSE"
      Just User -> withAttr winAttr $ C.hCenter $ str "YOU WON"
      Nothing -> withAttr drawAttr $ C.hCenter $ str "DRAW"
  | otherwise = emptyWidget

drawGrid :: Display -> Widget Name
drawGrid d = B.borderWithLabel (str "tic-tac-toe") $ vBox rows
  where
    g = d ^. game
    rows = [hBox $ cellsInRow r | r <- [g ^. size - 1,g ^. size - 2 .. 0]]
    cellsInRow y =
      [padLeftRight 1 $ drawCoord (V2 x y) | x <- [0 .. g ^. size - 1]]
    drawCoord cell =
      if (cell == d ^. selected)
        then withAttr selectedAttr $ cellAt cell
        else setAttr cell $ cellAt cell
    cellAt cell
      | cell `elem` g ^. crosses = str "X"
      | cell `elem` g ^. zeroes = str "O"
      | otherwise = str "."
    setAttr cell
      | cell == d ^. selected = withAttr selectedAttr
      | cell `elem` g ^. crosses = withAttr crossAttr
      | cell `elem` g ^. zeroes = withAttr zeroAttr
      | otherwise = withAttr emptyAttr

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
    , (loseAttr, fg V.red `V.withStyle` V.bold)
    , (winAttr, fg V.green `V.withStyle` V.bold)
    , (drawAttr, fg V.yellow `V.withStyle` V.bold)
    , (selectedAttr, bg V.yellow)
    ]
