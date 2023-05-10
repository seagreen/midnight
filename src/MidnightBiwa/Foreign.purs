module MidnightBiwa.Foreign where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (for)
import Foreign (Foreign)

-- * eval

foreign import _evalToForeign
  :: (forall x y. x -> Either x y)
  -> (forall x y. y -> Either x y)
  -> String
  -> Either String Foreign

evalToForeign :: String -> Either String Foreign
evalToForeign =
  _evalToForeign Left Right

-- | Giving this the short name, since it's used so much in the tests.
eval :: String -> Either String String
eval src =
  toString <$> evalToForeign src

-- * apply lambda

foreign import _applyClosure
  :: (forall x y. x -> Either x y)
  -> (forall x y. y -> Either x y)
  -> Foreign
  -> Array Foreign
  -> Either String Foreign

applyClosure :: Foreign -> Array Foreign -> Either String Foreign
applyClosure =
  _applyClosure Left Right

applyClosureFirstEvalingArgs :: Foreign -> Array String -> Either String Foreign
applyClosureFirstEvalingArgs l args = do
  argsForeign <- for args evalToForeign
  applyClosure l argsForeign

-- * toString

foreign import _toString
  :: Foreign
  -> String

toString :: Foreign -> String
toString =
  _toString

-- * array int

foreign import _toArrayInt
  :: Foreign
  -> Array Int

toArrayInt :: Foreign -> Array Int
toArrayInt =
  _toArrayInt
