module MidnightJS.AST where

import Debug
import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
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
  case _ of
    Var id ->
      id

    Lam params body ->
      "(("
        <> List.intercalate ", " params
        <> ") => "
        <> serialize body
        <> ")"

    LamVariadic param body ->
      "((..."
        <> param
        <> "$$" -- TODO
        <> ") => {const "
        <> param
        <> " = arrayToLinkedList("
        <> param
        <> "$$); "
        <> "return "
        <> serialize body
        <> ";})"

    LamUnitImmediateInvoked body ->
      "(() => " <> serialize body <> ")()"

    Let bindingList body ->
      serialize
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

    App f args ->
      serialize f <> "(" <> serializeCommaSeparated args <> ")"

    NilList ->
      "[]"

    Pair a b ->
      "[" <> serializeCommaSeparated (a : b : List.Nil) <> "]"

    JSString sym ->
      "\"" <> sym <> "\""

    Int n ->
      show n

    If predicate consequent alternative ->
      "("
        <> serialize predicate
        <> """ === "t" ? """
        <> serialize consequent
        <> " : "
        <> serialize alternative
        <> ")"

    JsBool b ->
      show b

    Block xs ->
      "{" <> List.intercalate "; " (serialize <$> xs) <> "}"

    JsLet name mVal ->
      case mVal of
        Nothing ->
          "let " <> name

        Just val ->
          "let " <> name <> " = " <> serialize val

    While cond body ->
      "while (" <> serialize cond <> ") " <> serialize body

    Not name ->
      "!" <> name

    Const name val ->
      "const " <> name <> " = " <> serialize val

    Assignment name val ->
      name <> " = " <> serialize val

    Return a ->
      "return " <> serialize a

    BareReturn ->
      "return"

    Throw e ->
      "throw " <> e

serializeCommaSeparated :: List AST -> String
serializeCommaSeparated xs =
  List.intercalate ", " (serialize <$> xs)

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
