{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Client
  ( main
  ) where

import           API                 (UserAPI, userAPI)
import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant
import           Servant.Client
import           Brick
import           Game
import           System.Random
import           UI                  (drawUI, gameAttrMap)
import Control.Lens
import Control.Monad.IO.Class
import qualified Graphics.Vty as V

sendNewGame :<|> sendMove :<|> sendFinishGame = client userAPI

clientEnv :: IO ClientEnv
clientEnv = do
  manager' <- newManager defaultManagerSettings
  return $ mkClientEnv manager' (BaseUrl Http "localhost" 2039 "")

mkReqMove :: Game -> ClientM Game
mkReqMove g = sendMove (g ^. gid) (g ^. selected)

mkReqNewgame :: Int -> Player -> ClientM Game
mkReqNewgame = sendNewGame

mkReqFinish :: Int -> ClientM ()
mkReqFinish = sendFinishGame

mkMove :: Game -> IO Game
mkMove g = do
  env <- clientEnv
  res <-
    runClientM
      (mkReqMove g)
      env
  case res of
    Left err    -> do
--       putStrLn "error on move"
--       putStrLn $ show g
      return g
    Right nGame -> do
--       putStrLn "OK move:"
--       putStrLn $ "request"  <> show g
--       putStrLn $ "response" <> show nGame
      return nGame

mkNewgame :: Int -> Player -> IO Game
mkNewgame size player = do
  env <- clientEnv
  res <-
    runClientM
      (mkReqNewgame size player)
      env
  case res of
    Left err    -> return $ error "Unable to make a new game"
    Right nGame -> return nGame


mkFinishGame :: Game -> IO ()
mkFinishGame game = do
  env <- clientEnv
  res <-
      runClientM
        (mkReqFinish $ game ^. gid)
        env
  case res of
    Left err -> return $ error "Unable to finish a game"
    Right () -> return ()


restartGame :: Game ->  IO Game
restartGame game
  | isGameOver game = do
      firstPlayer <- getFirstPlayer
      mkNewgame (game ^. size) firstPlayer
  | otherwise       = return game


handleEvent :: Game -> BrickEvent Name () -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = do
  res <- liftIO $ restartGame g
  continue $ res
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = do
  res <- liftIO $ mkMove g
  continue res
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = do
  liftIO $ mkFinishGame g
  halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
  liftIO $ mkFinishGame g
  halt g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ moveCursor UpDir g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) =
  continue $ moveCursor RightDir g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ moveCursor DownDir g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ moveCursor LeftDir g
handleEvent g _ = continue g


readSize :: IO Int
readSize = do
  putStrLn "Enter grid size (side of the square from 3 to 20):"
  side :: Int <- readLn
  if side < 3 || side > 20 then readSize else return side


main :: IO ()
main = do
  let app = App drawUI neverShowCursor handleEvent return (const gameAttrMap)
  side <- readSize
  firstPlayer <- getFirstPlayer
  pseudoGame <- mkNewgame side firstPlayer
  finalState <- defaultMain app pseudoGame
  putStrLn $ show pseudoGame
  putStrLn "Goodbye!"
