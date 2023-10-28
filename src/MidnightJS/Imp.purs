module MidnightJS.Imp where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import MidnightJS.AST (AST, LamParams(..))
import MidnightJS.AST as AST

type Id = String

-- | See the PureScript compiler for a good example of this type of thing.
data Imp
  = Var Id
  | Lam LamParams Imp
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
  | TceFunction String LamParams Imp
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

    Let bindingList body ->
      AST.Let
        ( (\(Tuple name val) -> Tuple name (toAST val))
            <$> bindingList
        )
        (toAST body)

    App f args ->
      AST.App (toAST f) (toAST <$> args)

    NilList ->
      AST.NilList

    Pair a b ->
      AST.Pair (toAST a) (toAST b)

    ImpString sym ->
      AST.JSString sym

    Int n ->
      AST.Int n

    If predicate consequent alternative ->
      AST.If (toAST predicate) (toAST consequent) (toAST alternative)

    TceFunction tce_metavar_name lamParams body ->
      let
        tceParams =
          AST.mapLamParams
            (\param -> tce_metavar_name <> "_" <> param)
            lamParams
      in
        AST.Lam tceParams
          ( AST.Block
              ( AST.JsLet (tce_metavar_name <> "_done") (Just (AST.JsBool false))
                  : AST.JsLet (tce_metavar_name <> "_result") Nothing
                  : AST.Const
                      (tce_metavar_name <> "_loop")
                      ( toAST
                          ( Lam
                              ( case tceParams of
                                  LamParamsFixed params ->
                                    LamParamsFixed params

                                  LamParamsVariadic param ->
                                    LamParamsFixed (List.singleton param)
                              )
                              body
                          )
                      )
                  : AST.While
                      (AST.Not (tce_metavar_name <> "_done"))
                      ( AST.Block
                          ( AST.Assignment
                              (tce_metavar_name <> "_result")
                              ( AST.App
                                  (AST.Var (tce_metavar_name <> "_loop"))
                                  ( case tceParams of
                                      LamParamsFixed params ->
                                        AST.Var <$> params

                                      LamParamsVariadic param ->
                                        List.singleton (AST.Var param)
                                  )
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
