module MidnightJS.AST where

import Debug
import Prelude

import Control.Monad.Except.Trans (ExceptT, except, runExceptT)
import Control.Monad.Trampoline (Trampoline, delay, done, runTrampoline)
import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))

type Id = String

data AST
  = Var Id
  | Lam (List String) AST
  | LamVariadic String AST
  | Let (List (Tuple String AST)) AST
  | App AST (List AST)
  | If AST AST AST
  | NilList
  | Pair AST AST
  | Int Int
  | JSString String
  | Throw String
  --
  -- Above is shared with Imp, below is JS specific.
  --
  | JsBool Boolean
  | Block (List AST)
  | JsLet String (Maybe AST)
  | While AST AST
  | Not String
  | Const String AST
  | Assignment String AST
  | Return AST
  | BareReturn
  | -- | immediately invoked function expression (IIFE)
    --
    -- Used to simulate lisp-style let.
    --
    -- `(() => <AST>)()`
    LamUnitImmediateInvoked AST

derive instance Eq AST
derive instance Generic AST _
instance Show AST where
  show a = genericShow a

serialize :: AST -> String
serialize =
  runTrampoline <<< serializeGo

serializeGo :: AST -> Trampoline String
serializeGo =
  case _ of
    Var id ->
      done id

    Lam params body -> do
      bodyStr <- serializeGo body
      done
        ( "(("
            <> List.intercalate ", " params
            <> ") => "
            <> bodyStr
            <> ")"
        )

    LamVariadic param body -> do
      bodyStr <- serializeGo body
      done
        ( "((..."
            <> param
            <> "$$" -- TODO
            <> ") => {const "
            <> param
            <> " = arrayToLinkedList("
            <> param
            <> "$$); "
            <> "return "
            <> bodyStr
            <> ";})"
        )

    LamUnitImmediateInvoked body -> do
      bodyStr <- serializeGo body
      done
        ( "(() => " <> bodyStr <> ")()"
        )

    Let bindingList body ->
      serializeGo
        ( LamUnitImmediateInvoked
            ( Block
                -- Const name (addLog name val)
                ( ( (\(Tuple name val) -> Const name val)
                      <$> bindingList
                  )
                    <> List.singleton (Return body)
                )
            )
        )

    App f args -> do
      fStr <- serializeGo f
      argsStr <- serializeCommaSeparated args
      done (fStr <> "(" <> argsStr <> ")")

    NilList ->
      done "[]"

    Pair a b -> do
      contentsStr <- serializeCommaSeparated (a : b : List.Nil)
      done ("[" <> contentsStr <> "]")

    JSString sym ->
      done ("\"" <> sym <> "\"")

    Int n ->
      done (show n)

    If predicate consequent alternative -> do
      predicateStr <- serializeGo predicate
      consequentStr <- serializeGo consequent
      alternativeStr <- serializeGo alternative
      done
        ( "("
            <> predicateStr
            <> """ === "t" ? """
            <> consequentStr
            <> " : "
            <> alternativeStr
            <> ")"
        )
    JsBool b ->
      done (show b)

    Block xs -> do
      strs <- for xs serializeGo
      done ("{" <> List.intercalate "; " strs <> "}")

    JsLet name mVal ->
      case mVal of
        Nothing ->
          done ("let " <> name)

        Just val -> do
          valStr <- serializeGo val
          done ("let " <> name <> " = " <> valStr)

    While cond body -> do
      condStr <- serializeGo cond
      bodyStr <- serializeGo body
      done ("while (" <> condStr <> ") " <> bodyStr)

    Not name ->
      done ("!" <> name)

    Const name val -> do
      valStr <- serializeGo val
      done ("const " <> name <> " = " <> valStr)

    Assignment name val -> do
      valStr <- serializeGo val
      done (name <> " = " <> valStr)

    Return a -> do
      aStr <- serializeGo a
      done ("return " <> aStr)

    BareReturn ->
      done "return"

    Throw e ->
      done ("throw " <> e)

serializeCommaSeparated :: List AST -> Trampoline String
serializeCommaSeparated xs = do
  strs <- for xs serializeGo
  done (List.intercalate ", " strs)

{-
addLog :: String -> AST -> AST
addLog name val =
  case val of
    Lam params body ->
      Lam params
        ( LamUnitImmediateInvoked
            ( Block
                (logStatement : Return (LamUnitImmediateInvoked body) : List.Nil)
            )
        )

    LamVariadic param body ->
      LamVariadic param
        ( LamUnitImmediateInvoked
            ( Block
                (logStatement : Return (LamUnitImmediateInvoked body) : List.Nil)
            )
        )

    _ ->
      val
  where
  logStatement :: AST
  logStatement =
    App (Var "console.log") (List.singleton (JSString ("+++" <> name)))

-}
