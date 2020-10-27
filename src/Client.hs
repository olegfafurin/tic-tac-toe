{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Client
  ( main
  ) where

import API (userAPI)
import Brick
import Control.Lens
import Control.Monad.IO.Class
import Game
import qualified Graphics.Vty as V
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client
import UI (drawUI, gameAttrMap)

sendNewGame :: Int -> Player -> ClientM Game
sendMove :: Int -> Cell -> ClientM Game
sendFinishGame :: Int -> ClientM ()

sendNewGame :<|> sendMove :<|> sendFinishGame = client userAPI

clientEnv :: IO ClientEnv
clientEnv = do
  manager' <- newManager defaultManagerSettings
  return $ mkClientEnv manager' (BaseUrl Http "localhost" 2039 "")

mkReqMove :: Display -> ClientM Game
mkReqMove d = sendMove (d ^. game . gid) (d ^. selected)

mkReqNewgame :: Int -> Player -> ClientM Game
mkReqNewgame = sendNewGame

mkReqFinish :: Int -> ClientM ()
mkReqFinish = sendFinishGame

mkMove :: Display -> IO Game
mkMove d = do
  env <- clientEnv
  res <- runClientM (mkReqMove d) env
  case res of
    Left _      -> return $ d ^. game
    Right nGame -> return nGame

mkNewgame :: Int -> Player -> IO Game
mkNewgame boardSize player = do
  env <- clientEnv
  res <- runClientM (mkReqNewgame boardSize player) env
  case res of
    Left _ -> return $ error "Unable to make a new game"
    Right nGame -> return nGame

mkFinishGame :: Display -> IO ()
mkFinishGame d = do
  env <- clientEnv
  res <- runClientM (mkReqFinish $ d ^. game . gid) env
  case res of
    Left _ -> return $ error "Unable to finish a game"
    Right () -> return ()

restartGame :: Game -> IO Game
restartGame g
  | isGameOver g = do
    firstPlayer <- getFirstPlayer
    mkNewgame (g ^. size) firstPlayer
  | otherwise = return g

handleEvent :: Display -> BrickEvent Name () -> EventM Name (Next Display)
handleEvent d (VtyEvent (V.EvKey (V.KChar 'r') [])) = do
  res <- liftIO $ restartGame (d ^. game) >>= (\g -> return $ d & game .~ g)
  continue $ res
handleEvent d (VtyEvent (V.EvKey V.KEnter [])) = do
  res <- liftIO $ mkMove d
  continue $ d & game .~ res
handleEvent d (VtyEvent (V.EvKey V.KEsc [])) = do
  liftIO $ mkFinishGame d
  halt d
handleEvent d (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
  liftIO $ mkFinishGame d
  halt d
handleEvent d (VtyEvent (V.EvKey V.KUp [])) = continue $ moveCursor UpDir d
handleEvent d (VtyEvent (V.EvKey V.KRight [])) =
  continue $ moveCursor RightDir d
handleEvent d (VtyEvent (V.EvKey V.KDown [])) = continue $ moveCursor DownDir d
handleEvent d (VtyEvent (V.EvKey V.KLeft [])) = continue $ moveCursor LeftDir d
handleEvent d _ = continue d

readSize :: IO Int
readSize = do
  putStrLn "Enter grid size (side of the square from 3 to 20):"
  side :: Int <- readLn
  if side < 3 || side > 20
    then readSize
    else return side

main :: IO ()
main = do
  let app = App drawUI neverShowCursor handleEvent return (const gameAttrMap)
  side <- readSize
  firstPlayer <- getFirstPlayer
  firstState <- mkNewgame side firstPlayer
  let half = firstState ^. size `div` 2
  let startCell = Cell half half
  _ <- defaultMain app (Display firstState $ startCell)
--   putStrLn $ show firstState
  putStrLn "Goodbye!"
