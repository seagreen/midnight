module MidnightJS.TCE where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import MidnightJS.AST (LamParams(..))
import MidnightJS.Imp (Imp(..))

{- Our goal:
```
(() => {
   const go = copy_n => {
      let tco_done = false;
      let tco_result;
      const tco_loop = n =>
        (n === 0)
          ? (() => {tco_done = true; return "a";})()
          : (() => {copy_n = n - 1; return;})()
      while (!tco_done) {
        tco_result = tco_loop(copy_n);
      }
      return tco_result;
   };
   return go(1000000);
}
)()
```
-}

{-
For example in PureScript this function:
```
slowConstZero :: Int -> Int
slowConstZero x =
  let
    go :: Int -> Int
    go n =
        if n == 0 then
            0
        else
            go (n - 1)
  in go x
```
results in this JS:
```
var slowConstZero = function (x) {
    var go = function ($copy_n) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(n) {
            var $32 = n === 0;
            if ($32) {
                $tco_done = true;
                return 0;
            };
            $copy_n = n - 1 | 0;
            return;
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($copy_n);
        };
        return $tco_result;
    };
    return go(x);
};
```
-}

-- | Boilerplate, other than the `tailCallCheck` call in `Let`.
tailCallElimation :: Imp -> Imp
tailCallElimation =
  case _ of
    Var id ->
      Var id

    Lam params body ->
      Lam params (tailCallElimation body)

    Let bindingList body ->
      Let
        ( (\(Tuple name expr) -> Tuple name (tailCallCheck name expr))
            <$> bindingList
        )
        (tailCallElimation body)

    App f args ->
      App (tailCallElimation f) (tailCallElimation <$> args)

    If predicate consequent alternative ->
      If
        (tailCallElimation predicate)
        (tailCallElimation consequent)
        (tailCallElimation alternative)

    NilList ->
      NilList

    Pair a b ->
      Pair (tailCallElimation a) (tailCallElimation b)

    Int n ->
      Int n

    ImpString str ->
      ImpString str

    -- NOTE: This should never be hit.
    -- Same for the next two arms of the case.
    TceFunction tce_metavar_name params body ->
      TceFunction tce_metavar_name params body

    TceBaseCase tce_metavar_name imp ->
      TceBaseCase tce_metavar_name imp

    TceRecursiveCall tce_metavar_name args ->
      TceRecursiveCall tce_metavar_name args

    Throw e ->
      Throw e

tailCallCheck :: String -> Imp -> Imp
tailCallCheck name expr =
  case expr of
    Lam lamParams body ->
      case lamParams of
        LamParamsFixed params ->
          -- TODO: also ensure doesn't appear in non-tail position
          -- (same for below)
          if inTailPosition name body then
            let
              tce_metavar_name = "$tce"
            in
              TceFunction
                tce_metavar_name
                params
                ( tceReturns
                    name
                    tce_metavar_name
                    params
                    -- The `tailCallElimination` call here is to support
                    -- tail recursive functions which themselves define other
                    -- tail recursive functions in their `let`s.
                    --
                    -- See the `nestedLetRecursiveExample` function in the tests.
                    -- Note that even after turning the argument to `nested-go` up to
                    -- 40000 in that test,
                    -- and removing the `tailCallElimination` invocation here,
                    -- the test still passes.
                    --
                    -- Not sure why that is, maybe a JS optimization is happening
                    -- due to the simplicity of `nested-go` that removes the tail recursion?
                    (tailCallElimation body)
                )

          else
            Lam lamParams (tailCallElimation body)

        LamParamsVariadic param ->
          if inTailPosition name body then
            -- TODO: implement here too:
            Lam (LamParamsVariadic param) (tailCallElimation body)
          else
            Lam (LamParamsVariadic param) (tailCallElimation body)

    _ ->
      tailCallElimation expr

tceReturns :: String -> String -> List String -> Imp -> Imp
tceReturns name tce_metavar_name params expr =
  case expr of
    Var _ ->
      baseCase

    Lam _ _ ->
      baseCase

    Let bindingList body ->
      Let bindingList (rec body)

    App f args ->
      case f of
        Var id ->
          if id == name then
            TceRecursiveCall tce_metavar_name (List.zip params args)
          else
            baseCase

        _ ->
          baseCase

    If predicate consequent alternative ->
      If
        predicate
        (rec consequent)
        (rec alternative)

    NilList ->
      baseCase

    Pair _ _ ->
      baseCase

    Int _ ->
      baseCase

    ImpString _ ->
      baseCase

    TceFunction _ _ _ ->
      baseCase

    TceBaseCase _ _ ->
      baseCase

    TceRecursiveCall _ _ ->
      baseCase

    Throw _ ->
      baseCase
  where
  rec :: Imp -> Imp
  rec =
    tceReturns name tce_metavar_name params

  baseCase :: Imp
  baseCase =
    TceBaseCase tce_metavar_name expr

inTailPosition :: String -> Imp -> Boolean
inTailPosition name =
  case _ of
    Var id ->
      id == name

    Lam lamParams body ->
      case lamParams of
        LamParamsFixed params ->
          if List.elem name params then
            false
          else
            inTailPosition name body

        LamParamsVariadic param ->
          if name == param then
            false
          else
            inTailPosition name body

    Let bindingList body ->
      if List.elem name (Tuple.fst <$> bindingList) then false
      else inTailPosition name body

    App f _ ->
      inTailPosition name f

    If _ consequent alternative ->
      inTailPosition name consequent
        || inTailPosition name alternative

    NilList ->
      false

    Pair _ _ ->
      false

    Int _ ->
      false

    ImpString _ ->
      false

    -- TODO: is this right?
    TceFunction _ _ _ ->
      false

    TceBaseCase _ _ ->
      false

    TceRecursiveCall _ _ ->
      false

    Throw _ ->
      false