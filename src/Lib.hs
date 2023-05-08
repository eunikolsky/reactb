{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

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

type API = "meals.json" :> Get '[JSON] Meals

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return defaultMeals

defaultMeals :: Meals
defaultMeals = Meals $ M.fromList
  [
    ("m1", Meal "Sushi" "Finest fish and veggies" 22.99),
    ("m2", Meal "Schnitzel" "A german specialty!" 16.5),
    ("m3", Meal "Barbecue Burger" "American, raw, meaty" 12.99),
    ("m4", Meal "Green Bowl" "Healthy...and green..." 18.99)
  ]
