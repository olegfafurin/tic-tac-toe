{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module API
    ( UserAPI
    , userAPI
    ) where

import Servant
import Game

type UserAPI = "newgame" :> QueryParam "size" Int :> QueryParam "seed" Int :> QueryParam "turn" Turn :> Get '[JSON] Game
          :<|> "move" :> QueryParam "game_id" Int :> QueryParam "cell" Cell :> Get '[JSON] Game

userAPI :: Proxy UserAPI
userAPI = Proxy