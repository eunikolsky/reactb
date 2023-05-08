{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson qualified as A
import Data.Aeson.TH
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Servant
import Test.RandomStrings

data Meal = Meal
  { name :: String
  , description :: String
  , price :: Double
  } deriving stock (Eq, Show)

$(deriveJSON defaultOptions ''Meal)

type MealKey = String

newtype Meals = Meals (Map MealKey Meal)
  deriving stock (Eq, Show)

$(deriveJSON defaultOptions ''Meals)

type OrderKey = String
type Order = A.Value

newtype Orders = Orders (Map OrderKey Order)
  deriving stock (Eq, Show)

$(deriveJSON defaultOptions ''Orders)

type API = "meals.json" :> Get '[JSON] Meals
  -- TODO in fact, firebase returns the key of the added object
  :<|> "orders.json" :> ReqBody '[JSON] A.Value :> Post '[JSON] NoContent
  :<|> "orders.json" :> Get '[JSON] Orders

type RuntimeState = MVar Orders

startApp :: IO ()
startApp = do
  state <- newMVar $ Orders M.empty
  run 8080 $ app state

app :: RuntimeState -> Application
app = logStdoutDev
  . cors (const $ Just policy)
  -- this is needed to support CORS preflight requests
  . provideOptions api
  . serve api
  . server
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = ["content-type"] }

api :: Proxy API
api = Proxy

server :: RuntimeState -> Server API
server state = getMeals :<|> postOrder state :<|> getOrders state

getMeals :: Handler Meals
getMeals = return defaultMeals

postOrder :: RuntimeState -> A.Value -> Handler NoContent
postOrder state order = liftIO $ do
  key <- randomWord (onlyAlphaNum randomASCII) 20
  (Orders orders) <- takeMVar state
  putMVar state . Orders $ M.insert key order orders
  return NoContent

getOrders :: RuntimeState -> Handler Orders
getOrders state = liftIO $ readMVar state

defaultMeals :: Meals
defaultMeals = Meals $ M.fromList
  [
    ("m1", Meal "Sushi" "Finest fish and veggies" 22.99),
    ("m2", Meal "Schnitzel" "A german specialty!" 16.5),
    ("m3", Meal "Barbecue Burger" "American, raw, meaty" 12.99),
    ("m4", Meal "Green Bowl" "Healthy...and green..." 18.99)
  ]
