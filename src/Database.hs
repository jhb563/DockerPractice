{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Monad.Logger (runStdoutLoggingT, LoggingT, filterLogger, LogLevel(..))
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 (pack, unpack)
import Database.Redis (ConnectInfo, defaultConnectInfo, runRedis, connect, setex, del)
import qualified Database.Redis as Redis
import Database.Persist.Postgresql (ConnectionString, Key, toSqlKey, withPostgresqlConn,
                                    SqlPersistT)
import Database.Persist (get, insert, Entity(..), delete)

import Schema (User)

type RedisInfo = ConnectInfo
type PGInfo = ConnectionString

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

fetchRedisConnection :: IO RedisInfo
fetchRedisConnection = return defaultConnectInfo

fetchPostgresConnection :: IO PGInfo
fetchPostgresConnection = return "host=localhost port=5432 user=admin dbname=my_db"

fetchUserRedis :: RedisInfo -> Int -> IO (Maybe User)
fetchUserRedis redisInfo uid = do
  connection <- connect redisInfo
  runRedis connection $ do
    res <- Redis.get (pack . show $ uid)
    case res of
      Right (Just userString) -> return $ Just (read . unpack $ userString)
      _ -> return Nothing

cacheUser :: RedisInfo -> Int -> User -> IO ()
cacheUser redisInfo uid user = do
  connection <- connect redisInfo
  runRedis connection $ do
    _ <- setex (pack . show $ uid) 3600 (pack . show $ user)
    return ()

deleteUserCache :: RedisInfo -> Int -> IO ()
deleteUserCache redisInfo uid = do
  connection <- connect redisInfo
  runRedis connection $ do
    _ <- del [pack . show $ uid]
    return ()

fetchUserPG :: PGInfo -> Int -> IO (Maybe User)
fetchUserPG connectionString uid = runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $
  runReaderT action
  where
    action :: SqlPersistT (LoggingT IO) (Maybe User)
    action = get $ toSqlKey (fromIntegral uid)
  

createUserPG :: PGInfo -> User -> IO (Key User)
createUserPG connectionString user = runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $
  runReaderT action
  where
    action :: SqlPersistT (LoggingT IO) (Key User)
    action = insert user

deleteUserPG :: PGInfo -> Int -> IO ()
deleteUserPG connectionString uid = runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $
  runReaderT action
  where
    action :: SqlPersistT (LoggingT IO) ()
    action = delete $ (toSqlKey (fromIntegral uid) :: Key User)
