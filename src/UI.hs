{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module UI
  ( handleEvent
  , gameAttrMap
  , drawUI
  , mkNewgame
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
import Servant.Types.SourceT (foreach)
import System.IO.Unsafe

sendNewGame :<|> sendMove = client userAPI

mkReqMove :: Game -> ClientM Game
mkReqMove g = sendMove (g ^. gid) (g ^. selected)

mkReqNewgame :: Int -> Int -> Turn -> ClientM Game
mkReqNewgame = sendNewGame

mkMove :: Game -> IO Game
mkMove g = do
  manager' <- newManager defaultManagerSettings
  res <-
    runClientM
      (mkReqMove g)
      (mkClientEnv manager' (BaseUrl Http "localhost" 2039 ""))
  case res of
    Left err -> return g
    Right nGame -> return nGame

mkNewgame :: Int -> Int -> Turn -> IO Game
mkNewgame size seed t = do
  manager' <- newManager defaultManagerSettings
  res <-
    runClientM
      (mkReqNewgame size seed t)
      (mkClientEnv manager' (BaseUrl Http "localhost" 2039 ""))
  case res of
    Left err -> return $ error "Unable to make a new game"
    Right nGame -> return nGame

handleEvent :: Game -> BrickEvent Name () -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = do
  res <- liftIO $ mkMove g
  continue res
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ moveCursor UpDir g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) =
  continue $ moveCursor RightDir g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ moveCursor DownDir g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ moveCursor LeftDir g
handleEvent g _ = continue g

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

main :: IO ()
main = do
  let app =
        App
          { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const gameAttrMap
          }
  putStrLn "Enter random seed:"
  (sd :: Int) <- readLn
  let initialState = newGame 0 10 sd Player
  putStrLn $ show initialState
  finalState <- defaultMain app initialState
  putStrLn "Done"
