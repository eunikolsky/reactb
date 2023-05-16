{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as A
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

-- | Generated key for an object submitted with `POST`.
newtype NewKey = NewKey String
  deriving stock (Eq, Show)

-- `deriving newtype (ToJSON)` doesn't work because it doesn't add the `name` field when `NewKey` had `name :: String`;
-- and `deriving stock (ToJSON)` doesn't work because `ToJSON` isn't a stock derivable class.
instance ToJSON NewKey where
  toJSON (NewKey key) = object ["name" .= key]

type API = "orders.json" :>
    ( ReqBody '[JSON] Object :> Post '[JSON] NewKey
    :<|> ReqBody '[JSON] Object :> Put '[JSON] Object
    :<|> Get '[JSON] Object
    )

type RuntimeState = MVar Object

startApp :: IO ()
startApp = do
  state <- newMVar mempty
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
server state = postOrder state :<|> putOrder state :<|> getOrders state

postOrder :: RuntimeState -> Object -> Handler NewKey
postOrder state order = liftIO $ do
  -- firebase push ids: https://gist.github.com/mikelehen/3596a30bd69384624c11
  key <- randomWord (onlyAlphaNum randomASCII) 20
  modifyMVar_ state $ \obj ->
    pure $ A.insert (AK.fromString key) (Object order) obj
  pure $ NewKey key

putOrder :: RuntimeState -> Object -> Handler Object
putOrder state obj = liftIO $ do
  modifyMVar_ state . const $ pure obj
  pure obj

getOrders :: RuntimeState -> Handler Object
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
