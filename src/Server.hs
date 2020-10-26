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
    , CellState (..)
    , Turn (..)
    , newGame
    , fixedGame
    , errGame
    , changeTurn
    , turn
    , starts
    , makeMove
    , crosses
    , zeroes
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
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.List
import Data.Sequence (Seq( (:<|) ) )



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
    let param :: Maybe (Game, Int) = do
            gameN <- gameId
            moveCell <- cell
            previousGame <- games !! gameN
            let uSign = case previousGame ^. starts of
                    Player   -> crosses
                    Computer -> zeroes
            updatedGame :: Game <- if moveCell `notElem` (previousGame ^. crosses) && moveCell `notElem` (previousGame ^. zeroes)
                                then Just (previousGame & uSign %~ ((:<|) moveCell))
                                else Nothing
            let compSign = case previousGame ^. starts of
                    Player   -> Zero
                    Computer -> Cross
            let nGame = unsafePerformIO (makeMove compSign updatedGame)
            return $ (nGame, gameN)
    liftIO $ case param of
            Just (game, num)  -> putMVar mvar (games & element num .~ Just game)
            Nothing -> putMVar mvar games
    case param of
            Just (game, num) -> return game
            Nothing          -> throwError err400


main :: IO ()
main = do
    mv <- newMVar ([Nothing :: Maybe Game | i <- [1..100]])
    let server :: Server UserAPI = (newGameHandler mv) :<|> (moveHandler mv)
    let app :: Application = serve userAPI server
    run 2039 app
