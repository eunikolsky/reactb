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
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as A
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Servant
import Test.RandomStrings

{-
https://firebase.google.com/docs/database/rest/start

We support only JSON objects (top-level) in the API.

firebase's API from the videos so far:
* `POST /foo.json` with json object: creates a unique key, stores the object under that key in the top-level object, and returns `{"name": "new_key"}`
* `PUT /foo.json` with json object: replaces the top-level object with the given object, and "the response will contain the data we wrote to the database" (apparently the entire object, like `GET`)
* `GET /foo.json`: returns the top-level object
-}

newtype Orders = Orders A.Object
  deriving stock (Eq, Show)

$(deriveJSON defaultOptions ''Orders)

type API
  = "orders.json" :> ReqBody '[JSON] A.Object :> Post '[JSON] NoContent
  :<|> "orders.json" :> Get '[JSON] Orders

type RuntimeState = MVar Orders

startApp :: IO ()
startApp = do
  -- newMVar mempty
  state <- newMVar $ Orders mempty
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
server state = postOrder state :<|> getOrders state

postOrder :: RuntimeState -> A.Object -> Handler NoContent
postOrder state order = liftIO $ do
  -- firebase push ids: https://gist.github.com/mikelehen/3596a30bd69384624c11
  key <- randomWord (onlyAlphaNum randomASCII) 20
  -- modifyMVar
  (Orders orders) <- takeMVar state
  putMVar state . Orders $ A.insert (AK.fromString key) (A.Object order) orders
  return NoContent

getOrders :: RuntimeState -> Handler Orders
getOrders state = liftIO $ readMVar state

{- TODO load default data from file
defaultMeals :: Meals
defaultMeals = Meals $ M.fromList
  [
    ("m1", Meal "Sushi" "Finest fish and veggies" 22.99),
    ("m2", Meal "Schnitzel" "A german specialty!" 16.5),
    ("m3", Meal "Barbecue Burger" "American, raw, meaty" 12.99),
    ("m4", Meal "Green Bowl" "Healthy...and green..." 18.99)
  ]
-}
