{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (killThread)
import Data.Maybe (isJust)
import Database.Persist.Postgresql (fromSqlKey)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientEnv(..), runClientM)
import Servant.Common.BaseUrl (parseBaseUrl)
import Test.Hspec

import API (fetchUserClient, createUserClient)
import Database (PGInfo, RedisInfo, fetchUserPG, deleteUserPG, fetchUserRedis, deleteUserCache)
import Schema (User(..))
import Utils

main :: IO ()
main = do
  (pgInfo, redisInfo, tid) <- setupTests
  mgr <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8000"
  let clientEnv = ClientEnv mgr baseUrl
  hspec $ before (beforeHook1 clientEnv pgInfo redisInfo) spec1
  hspec $ before (beforeHook2 clientEnv pgInfo redisInfo) $ after (afterHook2 pgInfo redisInfo) $ spec2
  hspec $ before (beforeHook3 clientEnv pgInfo redisInfo) $ after (afterHook3 pgInfo redisInfo) $ spec3
  killThread tid 

testUser :: User
testUser = User
  { userName = "james"
  , userEmail = "james@test.com"
  , userAge = 25
  , userOccupation = "Software Engineer"
  }

-- Fetch on empty DB results in no user in DB nor cache
-- Before Hook: Calls "fetch", Fetches DB, Fetches Cache, returns Bools
-- After Hook: None
beforeHook1 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool)
beforeHook1 clientEnv pgInfo redisInfo = do
  _ <- runClientM (fetchUserClient 1) clientEnv
  inPG <- isJust <$> fetchUserPG pgInfo 1
  inRedis <- isJust <$> fetchUserRedis redisInfo 1
  return (inPG, inRedis)

spec1 :: SpecWith (Bool, Bool)
spec1 = describe "After fetching on an empty database" $ do
  it "There should be no user in Postgres" $ \(inPG, _) -> inPG `shouldBe` False
  it "There should be no user in Redis" $ \(_, inRedis) -> inRedis `shouldBe` False

-- After create, user is in DB but not the cache
-- Before Hook: Calls "create", Fetches DB, Fetches Cache, returns Bools
-- After Hook: Delete user returned by creation
beforeHook2 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Int)
beforeHook2 clientEnv pgInfo redisInfo = do
  userKeyEither <- runClientM (createUserClient testUser) clientEnv
  case userKeyEither of
    Left _ -> error "DB call failed on spec 2!"
    Right userKey -> do 
      let intKey = fromIntegral $ fromSqlKey userKey
      inPG <- isJust <$> fetchUserPG pgInfo intKey
      inRedis <- isJust <$> fetchUserRedis redisInfo intKey
      return (inPG, inRedis, intKey)

spec2 :: SpecWith (Bool, Bool, Int)
spec2 = describe "After creating the user but not fetching" $ do
  it "There should be a user in Postgres" $ \(inPG, _, _) -> inPG `shouldBe` True
  it "There should be no user in Redis" $ \(_, inRedis, _) -> inRedis `shouldBe` False

afterHook2 :: PGInfo -> RedisInfo -> (Bool, Bool, Int) -> IO ()
afterHook2 pgInfo redisInfo (_, _, key) = do
  deleteUserCache redisInfo key
  deleteUserPG pgInfo key

-- After create AND fetch, user is in both DB and cache
-- Before Hook: Calls "create", Calls "fetch", Fetches DB, Fetches Cache
-- After Hook: Clear Cache, Clear DB
beforeHook3 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Int)
beforeHook3 clientEnv pgInfo redisInfo = do
  userKeyEither <- runClientM (createUserClient testUser) clientEnv
  case userKeyEither of
    Left _ -> error "DB call failed on spec 3!"
    Right userKey -> do 
      let intKey = fromIntegral $ fromSqlKey userKey
      _ <- runClientM (fetchUserClient intKey) clientEnv 
      inPG <- isJust <$> fetchUserPG pgInfo intKey
      inRedis <- isJust <$> fetchUserRedis redisInfo intKey
      return (inPG, inRedis, intKey)

spec3 :: SpecWith (Bool, Bool, Int)
spec3 = describe "After creating the user and fetching" $ do
  it "There should be a user in Postgres" $ \(inPG, _, _) -> inPG `shouldBe` True
  it "There should be a user in Redis" $ \(_, inRedis, _) -> inRedis `shouldBe` True

afterHook3 :: PGInfo -> RedisInfo -> (Bool, Bool, Int) -> IO ()
afterHook3 pgInfo redisInfo (_, _, key) = do
  deleteUserCache redisInfo key
  deleteUserPG pgInfo key
