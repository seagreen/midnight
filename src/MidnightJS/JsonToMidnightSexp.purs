module MidnightJS.JsonToMidnightSexp where

import Prelude

import Control.Monad.Except.Trans (ExceptT, except, runExceptT)
import Control.Monad.Trampoline (Trampoline, delay, done, runTrampoline)
import Data.Argonaut (Json, caseJson, stringify)
import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp

jsonToMidnightSexp :: Json -> Either String Sexp
jsonToMidnightSexp json = do
  runTrampoline (runExceptT (flattenSexpListsGo =<< (jsonToMidnightSexpNoFlattenGo json)))

jsonToMidnightSexpNoFlatten :: Json -> Either String Sexp
jsonToMidnightSexpNoFlatten json =
  runTrampoline (runExceptT (jsonToMidnightSexpNoFlattenGo json))

flattenSexpLists :: Sexp -> Either String Sexp
flattenSexpLists sexp =
  runTrampoline (runExceptT (flattenSexpListsGo sexp))

-- * Trampolined versions

jsonToMidnightSexpNoFlattenGo :: Json -> ExceptT String Trampoline Sexp
jsonToMidnightSexpNoFlattenGo json =
  caseJson
    ( \_ ->
        except (Left "unexpected null")
    )
    ( \b ->
        except (Left ("unexpected bool: " <> show b))
    )
    ( \n -> case fromNumber n of
        Nothing ->
          except (Left ("not an integer: " <> show n))
        Just int ->
          pure (Sexp.Int int)
    )
    (pure <<< Sexp.Symbol)
    ( \xs -> do
        res <- for xs jsonToMidnightSexpNoFlattenGo
        pure (Sexp.List (List.fromFoldable res))
    )
    ( \_ ->
        except (Left ("unexpected object: " <> stringify json))
    )
    json

flattenSexpListsGo :: Sexp -> ExceptT String Trampoline Sexp
flattenSexpListsGo sexp =
  case sexp of
    Sexp.List _ ->
      Sexp.List <$> flattenListGo sexp

    _ ->
      pure sexp

flattenListGo :: Sexp -> ExceptT String Trampoline (List Sexp)
flattenListGo xs =
  case xs of
    Sexp.List List.Nil ->
      pure List.Nil

    Sexp.List (car : cdr : List.Nil) -> do
      next <- flattenSexpListsGo car
      rest <- flattenListGo cdr
      pure (List.Cons next rest)

    _ ->
      except
        ( Left
            ( "flattenList expected a pair, but got: "
                <> show xs
            )
        )
