{-# LANGUAGE OverloadedStrings #-}

module UI where


import Game
import Control.Lens hiding ((<|), (|>), (:>), (:<), Empty)
import Control.Concurrent.MVar
import Linear.V2 (V2(..), _x, _y)
import Brick (App (..), AttrMap, AttrName, Widget, Next, BrickEvent(..), EventM, defaultMain, padLeftRight, hBox, vBox, withAttr, neverShowCursor, str, attrMap, on, continue, halt)
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS


handleEvent :: Game -> BrickEvent Name () -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ move UpDir g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ move RightDir g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ move DownDir g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ move LeftDir g
handleEvent g _                                     = continue g



drawUI :: Game -> [Widget Name]
drawUI g = [C.center $ drawGrid g]

drawGrid :: Game -> Widget Name
drawGrid g = B.borderWithLabel (str "tic-tac-toe")
            $ vBox rows
            where
                rows         = [hBox $ cellsInRow r | r <- [g^.size-1,g^.size-2..0]]
                cellsInRow y = [padLeftRight 1 $ drawCoord (V2 x y) | x <- [0..g^.size-1]]
                drawCoord    = drawCell . cellAt
                cellAt c
                  | c `elem` g ^. crosses = Cross
                  | c `elem` g ^. zeroes  = Zero
                  | c == g ^. selected    = Selected
                  | otherwise             = EmptyCell

drawCell :: CellState -> Widget Name
drawCell state = case state of
    EmptyCell -> withAttr emptyAttr (str ".")
    Cross -> withAttr crossAttr (str "X")
    Zero -> withAttr zeroAttr (str "O")
    Selected -> withAttr selectedAttr (str " ")


emptyAttr, crossAttr, zeroAttr, selectedAttr :: AttrName
crossAttr = "cross"
zeroAttr = "zero"
selectedAttr = "selected"
emptyAttr = "empty"

gameAttrMap = attrMap V.defAttr
  [ (crossAttr, V.red `on` V.black)
  , (zeroAttr, V.blue `on` V.black)
  , (selectedAttr, V.black `on` V.yellow)
  ]


main :: IO ()
main = do
    let app = App { appDraw = drawUI
                  , appChooseCursor = neverShowCursor
                  , appHandleEvent = handleEvent
                  , appStartEvent = return
                  , appAttrMap = const gameAttrMap }
    let initialState = newGame 10 Player
    putStrLn $ show initialState
    finalState <- defaultMain app initialState
    putStrLn "Done"