{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import API (UserAPI, userAPI)
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class
import Data.List
import Game
import Network.Wai.Handler.Warp
import Servant

newGameHandler :: (MVar [Maybe Game]) -> Int -> Player -> Handler Game
newGameHandler mvar boardSize turn = do
  games <- liftIO $ takeMVar mvar
  let freeSpot = elemIndex Nothing games
  let updatedGame :: Maybe Game = do
        f <- freeSpot
        return $ newGame f boardSize turn
  let newState =
        case freeSpot of
          Nothing -> games
          Just gameN -> games & element gameN .~ updatedGame
  liftIO $ putMVar mvar (newState)
  case freeSpot of
    Just _ ->
      case updatedGame of
        Just g -> return g
        Nothing -> throwError err400
    Nothing -> throwError err503

moveHandler :: (MVar [Maybe Game]) -> Int -> Cell -> Handler Game
moveHandler mvar gameId cell = do
  games <- liftIO $ takeMVar mvar
  let param :: Maybe (Game, Bool) = do
        previousGame <- games !! gameId
        let uSign =
              case previousGame ^. starts of
                User -> crosses
                Computer -> zeroes
        updatedGame :: Game <-
          if cell `notElem` (previousGame ^. crosses) &&
             cell `notElem` (previousGame ^. zeroes)
            then Just (previousGame & uSign %~ ((:) cell))
            else Nothing
        case getWinner updatedGame of
          Just User -> return (updatedGame & winner .~ Just User, True)
          Just Computer -> error "Comp wins on client's move"
          Nothing -> do
            let responseGame = makeMove updatedGame
            case getWinner responseGame of
              Just Computer ->
                return (responseGame & winner .~ Just Computer, True)
              Just User -> error "User wins on comp's move"
              Nothing -> return (responseGame, False)
  liftIO $
    case param of
      Just (g, False) -> putMVar mvar (games & element gameId .~ Just g)
      Just (g, True) -> do
        putStrLn $ "game is to be deleted:" <> show (g ^. gid)
        putMVar mvar (games & element gameId .~ Nothing)
      Nothing -> putMVar mvar games
  case param of
    Just (g, _) -> return g
    Nothing -> throwError err400

finishHandler :: (MVar [Maybe Game]) -> Int -> Handler ()
finishHandler mvar gameId = do
  games <- liftIO $ takeMVar mvar
  liftIO $ putMVar mvar (games & element gameId .~ Nothing)
  return ()

main :: IO ()
main = do
  mv <- newMVar ([Nothing :: Maybe Game | _ <- [1 .. 100 :: Int]])
  let server :: Server UserAPI =
        (newGameHandler mv) :<|> (moveHandler mv) :<|> (finishHandler mv)
  let app :: Application = serve userAPI server
  run 2039 app
