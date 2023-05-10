module Lib.Moore where

import Prelude

import Data.Foldable (foldl)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, dimap)
import Data.Tuple (Tuple(..), fst)

newtype Moore i o = Moore
  { output :: o
  , step :: (i -> Moore i o)
  }

derive instance Newtype (Moore i o) _

derive instance Functor (Moore i)

instance Profunctor Moore where
  dimap :: forall i o i2 o2. (i2 -> i) -> (o -> o2) -> Moore i o -> Moore i2 o2
  dimap g f (Moore m) =
    Moore
      { output: f m.output
      , step: \i2 -> dimap g f (m.step (g i2))
      }

stepMultiple :: forall i o. Moore i o -> List i -> List o
stepMultiple m inputs =
  List.reverse (fst (foldl f (Tuple mempty m) inputs))
  where
  f :: Tuple (List o) (Moore i o) -> i -> Tuple (List o) (Moore i o)
  f (Tuple acc (Moore m2)) i =
    let
      Moore res = m2.step i
    in
      Tuple (res.output : acc) (Moore res)

stepMultipleUnlessPred :: forall i o found. Moore i o -> (o -> Maybe found) -> List i -> Maybe found
stepMultipleUnlessPred (Moore m) f inputs =
  case inputs of
    List.Nil ->
      Nothing

    x : xs ->
      let
        Moore res = m.step x
      in
        case f res.output of
          Just a ->
            Just a

          Nothing ->
            stepMultipleUnlessPred (Moore { output: res.output, step: res.step }) f xs

-- | From https://hackage.haskell.org/package/machines-0.7.3/docs/Data-Machine-Moore.html#v:unfoldMoore
unfoldMoore :: forall s i o. (s -> Tuple o (i -> s)) -> s -> Moore i o
unfoldMoore f s =
  go s
  where
  go :: s -> Moore i o
  go s' =
    let
      Tuple o g = f s'
    in
      Moore
        { output: o
        , step: go <<< g
        }
