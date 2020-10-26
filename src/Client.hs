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
import qualified Graphics.Vty as V
-- import Brick
--   ( App(..)
--   , AttrMap
--   , AttrName
--   , BrickEvent(..)
--   , EventM
--   , Next
--   , Widget
--   , attrMap
--   , continue
--   , defaultMain
--   , hBox
--   , halt
--   , neverShowCursor
--   , on
--   , padLeftRight
--   , str
--   , vBox
--   , withAttr
--   )
import           Brick
import           Game
import           System.Random
import           UI                  (drawUI, gameAttrMap)
import Control.Lens
import Control.Monad.IO.Class

sendNewGame :<|> sendMove :<|> sendFinishGame = client userAPI

clientEnv :: IO ClientEnv
clientEnv = do
  manager' <- newManager defaultManagerSettings
  return $ mkClientEnv manager' (BaseUrl Http "localhost" 2039 "")

mkReqMove :: Game -> ClientM Game
mkReqMove g = sendMove (g ^. gid) (g ^. selected)

mkReqNewgame :: Int -> Int -> Player -> ClientM Game
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
    Left err    -> return g
    Right nGame -> return nGame

mkNewgame :: Int -> Int -> Player -> IO Game
mkNewgame size seed t = do
  env <- clientEnv
  res <-
    runClientM
      (mkReqNewgame size seed t)
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

handleEvent :: Game -> BrickEvent Name () -> EventM Name (Next Game)
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


main :: IO ()
main = do
  let app = App drawUI neverShowCursor handleEvent return (const gameAttrMap)
  putStrLn "Enter grid size (side of the square):"
  side :: Int <- readLn
  randomSeed <- (randomIO :: IO Int)
  randomTurn <- (randomIO :: IO Bool)
  let firstPlayer = case randomTurn of
                      True  -> Computer
                      False -> User
  pseudoGame <- mkNewgame side randomSeed firstPlayer
  finalState <- defaultMain app pseudoGame
  putStrLn $ show pseudoGame
  putStrLn "Goodbye!"
