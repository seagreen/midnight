module MidnightJS.Imp where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
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
  | Array (List Imp)
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
  case _ of
    Var id ->
      AST.Var id

    Lam params body ->
      AST.Lam params (toAST body)

    LamVariadic param body ->
      AST.LamVariadic param (toAST body)

    Let bindingList body ->
      AST.Let
        ( (\(Tuple name val) -> Tuple name (toAST val))
            <$> bindingList
        )
        (toAST body)

    App f params ->
      AST.App (toAST f) (toAST <$> params)

    Array xs ->
      AST.Array (toAST <$> xs)

    ImpString sym ->
      AST.JSString sym

    Int n ->
      AST.JSInt n

    If predicate consequent alternative ->
      AST.If (toAST predicate) (toAST consequent) (toAST alternative)

    TceFunction tce_metavar_name params body ->
      let
        tceParams =
          (\param -> tce_metavar_name <> "_" <> param)
            <$> params
      in
        AST.Lam tceParams
          ( AST.Block
              ( AST.JsLet (tce_metavar_name <> "_done") (Just (AST.JsBool false))
                  : AST.JsLet (tce_metavar_name <> "_result") Nothing
                  : AST.Const (tce_metavar_name <> "_loop") (toAST body)
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

    TceBaseCase tce_metavar_name imp ->
      AST.LamUnitImmediateInvoked
        ( AST.Block
            ( (AST.Assignment (tce_metavar_name <> "_done") (AST.JsBool true))
                : AST.Return (toAST imp)
                : List.Nil
            )
        )

    TceRecursiveCall tce_metavar_name args ->
      let
        f :: Tuple String Imp -> AST
        f (Tuple param imp) =
          AST.Assignment (tce_metavar_name <> "_" <> param) (toAST imp)

      in
        AST.LamUnitImmediateInvoked
          ( AST.Block
              ( (f <$> args)
                  <> List.singleton AST.BareReturn
              )
          )

    Throw e ->
      AST.Throw e
