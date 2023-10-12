module MidnightJS.Imp where

import Prelude

import Control.Monad.Except.Trans (ExceptT, except, runExceptT)
import Control.Monad.Trampoline (Trampoline, delay, done, runTrampoline)
import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import MidnightJS.AST (AST)
import MidnightJS.AST as AST

type Id = String

-- | See the PureScript compiler for a good example of this type of thing.
data Imp
  = Var Id
  | Lam (List String) Imp
  | LamVariadic String Imp
  | Let (List (Tuple String Imp)) Imp
  | App Imp (List Imp)
  | If Imp Imp Imp
  | NilList
  | Pair Imp Imp
  | Int Int
  | ImpString String
  | Throw String
  --
  -- Generated during optimization
  --
  | TceFunction String (List String) Imp
  | TceBaseCase String Imp
  | TceRecursiveCall String (List (Tuple String Imp))

derive instance Eq Imp
derive instance Generic Imp _
instance Show Imp where
  show a = genericShow a

toAST :: Imp -> AST
toAST =
  runTrampoline <<< toASTGo

toASTGo :: Imp -> Trampoline AST
toASTGo =
  case _ of
    Var id ->
      done (AST.Var id)

    Lam params body ->
      AST.Lam params <$> toASTGo body

    LamVariadic param body ->
      AST.LamVariadic param <$> toASTGo body

    Let bindingList body -> do
      bindingListAst <-
        for bindingList
          ( \(Tuple name val) ->
              Tuple name <$> toASTGo val
          )
      bodyAst <- toASTGo body
      done (AST.Let bindingListAst bodyAst)

    App f args -> do
      fAst <- toASTGo f
      argsAst <- for args toASTGo
      done (AST.App fAst argsAst)

    NilList ->
      done AST.NilList

    Pair a b ->
      AST.Pair <$> toASTGo a <*> toASTGo b

    ImpString sym ->
      done (AST.JSString sym)

    Int n ->
      done (AST.Int n)

    If predicate consequent alternative ->
      AST.If
        <$> toASTGo predicate
        <*> toASTGo consequent
        <*> toASTGo alternative

    TceFunction tce_metavar_name params body -> do
      let
        tceParams =
          (\param -> tce_metavar_name <> "_" <> param)
            <$> params
      bodyAst <- toASTGo body
      done
        ( AST.Lam tceParams
            ( AST.Block
                ( AST.JsLet (tce_metavar_name <> "_done") (Just (AST.JsBool false))
                    : AST.JsLet (tce_metavar_name <> "_result") Nothing
                    : AST.Const (tce_metavar_name <> "_loop") bodyAst
                    : AST.While
                        (AST.Not (tce_metavar_name <> "_done"))
                        ( AST.Block
                            ( AST.Assignment
                                (tce_metavar_name <> "_result")
                                ( AST.App
                                    (AST.Var (tce_metavar_name <> "_loop"))
                                    (AST.Var <$> tceParams)
                                ) : List.Nil
                            )
                        )
                    : AST.Return (AST.Var (tce_metavar_name <> "_result"))
                    : List.Nil
                )
            )
        )

    TceBaseCase tce_metavar_name imp -> do
      ast <- toASTGo imp
      done
        ( AST.LamUnitImmediateInvoked
            ( AST.Block
                ( (AST.Assignment (tce_metavar_name <> "_done") (AST.JsBool true))
                    : AST.Return ast
                    : List.Nil
                )
            )
        )

    TceRecursiveCall tce_metavar_name statements -> do
      let
        f :: Tuple String Imp -> Trampoline AST
        f (Tuple param imp) =
          AST.Assignment (tce_metavar_name <> "_" <> param) <$> toASTGo imp

      statementsAst <- for statements f
      done
        ( AST.LamUnitImmediateInvoked
            ( AST.Block
                ( statementsAst
                    <> List.singleton AST.BareReturn
                )
            )
        )

    Throw e ->
      done (AST.Throw e)
