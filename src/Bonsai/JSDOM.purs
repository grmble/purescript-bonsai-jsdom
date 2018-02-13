-- | Bonsai JSDOM module
-- |
-- | Makes it possible to use Bonsai.DOM (and Bonsai)
-- | with JSDOM for testing or server side rendering.
module Bonsai.JSDOM
  ( fireClick
  , jsdomDocument
  , jsdomWindow
  , setProperty
  , setValue
  , simulantFire
  )
where

import Prelude

import Bonsai.DOM (Document, Element, Window(..), document, failNullOrUndefined)
import Data.Foreign (F, Foreign, toForeign)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn2, runFn3)


foreign import primitives ::
  { jsdomWindow :: Fn1 String Foreign
  , simulantFire :: Fn2 String Element Unit
  , setProperty :: Fn3 String Foreign Element Unit
  }


-- | Create a JSDOM Window
jsdomWindow :: String -> F Window
jsdomWindow html =
  primitives.jsdomWindow html #
  failNullOrUndefined "jsdomWindow" >>=
  pure <<< Window

-- | Create a JSDOM Document (not returning the window)
jsdomDocument :: String -> F Document
jsdomDocument html =
  jsdomWindow html >>= document


-- | Set a named property on the element.
setProperty :: forall a. String -> a -> Element -> F Unit
setProperty name value elem =
  pure $ runFn3 primitives.setProperty name (toForeign value) elem


-- | Set the value property on the element.
setValue :: String -> Element -> F Unit
setValue =
  setProperty "value"


-- | Fire an event using simulant.
simulantFire :: String -> Element -> F Unit
simulantFire ev elem =
  pure $ runFn2 primitives.simulantFire ev elem


fireClick :: Element -> F Unit
fireClick =
  simulantFire "click"
