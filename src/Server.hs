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

type UserAPI = "newgame" :> QueryParam "size" Int :> QueryParam "turn" Turn :> Get '[JSON] Int
          :<|> "move" :> QueryParam "game_id" Int :> QueryParam "cell" Cell :> Get '[JSON] Game
          :<|> "some" :> QueryParam "lol" Int :> Get '[JSON] Game


data Load = Load { a :: IORef [Business]}
data Business = Busy | Free

-- makeState :: IO Load
-- makeState = do
--     ref <- newIORef [Free | i <- [1..100]]
--     return $ Load ref


newGameHandler :: (MVar [Maybe Game]) -> Maybe Int -> Maybe Turn -> Handler Int
newGameHandler mvar size turn = do
    games <- liftIO $ takeMVar mvar
    let freeSpot = elemIndex Nothing games
    let newState = case freeSpot of
            Nothing -> games
            Just gameN -> games & element gameN .~ Just (newGame (fromJust size) (fromJust turn))
    liftIO $ putMVar mvar (newState)
    if isJust freeSpot then return $ fromJust freeSpot else throwError err503


moveHandler :: (MVar [Maybe Game]) -> Maybe Int -> Maybe Cell -> Handler Game
moveHandler mvar gameId cell = do
    games <- liftIO $ takeMVar mvar
    let gameN = fromJust gameId
    let myGame = games !! gameN
    let updatedGame = case myGame of
            Nothing -> myGame
            Just someGame -> Just $ someGame & turn %~ changeTurn
    let newState = games & element gameN .~  updatedGame
    liftIO $ putMVar mvar newState
    return $ if isJust updatedGame then fromJust updatedGame else errGame

userAPI :: Proxy UserAPI
userAPI = Proxy

someHandler :: (Maybe Int) -> Handler Game
someHandler x = return errGame


main :: IO ()
main = do
    mv <- newMVar ([Nothing :: Maybe Game | i <- [1..100]])
    let server :: Server UserAPI = (newGameHandler mv) :<|> (moveHandler mv) :<|> someHandler
    let app :: Application = serve userAPI server
    run 2039 app
