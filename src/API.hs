{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module API
    ( UserAPI
    , userAPI
    ) where

import Servant
import Game

type UserAPI = "newgame" :> Capture "size" Int :> Capture "seed" Int :> Capture "turn" Turn :> Get '[JSON] Game
          :<|> "move" :> Capture "game_id" Int :> Capture "cell" Cell :> Get '[JSON] Game

userAPI :: Proxy UserAPI
userAPI = Proxy