-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

-- | Type synonym for an application model
data Model = Model { ccount :: Int,
                     fibs :: [Int]
                   } deriving (Show, Eq)

-- | Sum type for application events
data Action
  = AddOne
  | NoOp
  deriving (Show, Eq)

fibo :: [Int]
fibo = Prelude.take 100 $ 1:1:Prelude.zipWith (+) fibo (Prelude.tail fibo)  -- is ok
-- fibo = 1:1:Prelude.zipWith (+) fibo (Prelude.tail fibo)  -- not ok

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model  = Model {ccount = 0, fibs = fibo}                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff m {ccount = (ccount m + 1)}
updateModel NoOp m = noEff m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
 -- , text (ms $ Prelude.head $ fibs x)
   button_ [ onClick AddOne ] [ text "+" ]
 , text (ms $ ccount x)
 ]
