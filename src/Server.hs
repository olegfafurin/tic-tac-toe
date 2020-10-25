{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Server where

import Game
    ( Game(..)
    , Cell
    , Turn (..)
    , newGame
    , fixedGame
    , errGame
    , changeTurn
    , turn
    )
import API (userAPI, UserAPI)
import Servant
import Linear.V2 (V2(..), _x, _y)
import Network.Wai.Handler.Warp
import GHC.Generics
import System.IO.Unsafe
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Concurrent.Map
import Control.Concurrent.MVar
import Control.Lens
import Data.IORef
import Data.Maybe
import Data.List



newGameHandler :: (MVar [Maybe Game]) -> Maybe Int -> Maybe Int -> Maybe Turn -> Handler Game
newGameHandler mvar size seed turn = do
    games <- liftIO $ takeMVar mvar
    let freeSpot = elemIndex Nothing games
    let updatedGame :: Maybe Game = do
            f <- freeSpot
            s <- size
            sd <- seed
            t <- turn
            return $ newGame f s sd t
    let newState = case freeSpot of
            Nothing -> games
            Just gameN -> games & element gameN .~ updatedGame
    liftIO $ putMVar mvar (newState)
    case freeSpot of
            Just _  -> case updatedGame of
                    Just game -> return game
                    Nothing   -> throwError err400
            Nothing -> throwError err503


moveHandler :: (MVar [Maybe Game]) -> Maybe Int -> Maybe Cell -> Handler Game
moveHandler mvar gameId cell = do
    games <- liftIO $ takeMVar mvar
    let gameN = fromJust gameId
    let previousGame = games !! gameN
    let updatedGame = case previousGame of
            Nothing -> myGame
            Just someGame -> Just $ someGame & turn %~ changeTurn
    let newState = games & element gameN .~  updatedGame
    liftIO $ putMVar mvar newState
    case updatedGame
    return $ if isJust updatedGame then fromJust updatedGame else errGame


main :: IO ()
main = do
    mv <- newMVar ([Nothing :: Maybe Game | i <- [1..100]])
    let server :: Server UserAPI = (newGameHandler mv) :<|> (moveHandler mv) :<|> someHandler
    let app :: Application = serve userAPI server
    run 2039 app
