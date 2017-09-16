{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server

import qualified Database as D
import           Schema

type UsersAPI = 
       "users" :> Capture "userid" Int :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] (Key User)

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

fetchUsersHandler :: D.PGInfo -> D.RedisInfo -> Int -> Handler User
fetchUsersHandler pgInfo redisInfo uid = do
  maybeCachedUser <- liftIO $ D.fetchUserRedis redisInfo uid 
  case maybeCachedUser of
    Just user -> return user
    Nothing -> do
      maybeDBUser <- liftIO $ D.fetchUserPG pgInfo uid
      case maybeDBUser of
        Just user -> liftIO (D.cacheUser redisInfo uid user) >> return user
        Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

createUserHandler :: D.PGInfo -> D.RedisInfo -> User -> Handler (Key User)
createUserHandler pgInfo redisInfo user = liftIO $ D.createUserPG pgInfo user

usersServer :: D.PGInfo -> D.RedisInfo -> Server UsersAPI
usersServer pgInfo redisInfo = 
  (fetchUsersHandler pgInfo redisInfo) :<|> 
  (createUserHandler pgInfo redisInfo)


runServer :: IO ()
runServer = do
  pgInfo <- D.fetchPostgresConnection
  redisInfo <- D.fetchRedisConnection
  run 8000 (serve usersAPI (usersServer pgInfo redisInfo))

fetchUserClient :: Int -> ClientM User
createUserClient :: User -> ClientM (Key User)
(fetchUserClient :<|> createUserClient) = client (Proxy :: Proxy UsersAPI)
