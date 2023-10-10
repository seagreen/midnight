module MidnightJS.AST where

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
  | Array (List AST)
  | JSInt Int
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
        <> ") => "
        <> serialize body
        <> ")"

    LamUnitImmediateInvoked body ->
      "(() => " <> serialize body <> ")()"

    Let bindingList body ->
      serialize
        ( LamUnitImmediateInvoked
            ( Block
                ( ( (\(Tuple name val) -> Const name val)
                      <$> bindingList
                  )
                    <> List.singleton (Return body)
                )
            )
        )

    App f params ->
      serialize f <> "(" <> serializeCommaSeparated params <> ")"

    Array xs ->
      "[" <> serializeCommaSeparated xs <> "]"

    JSString sym ->
      "\"" <> sym <> "\""

    JSInt n ->
      show n

    If predicate consequent alternative ->
      serialize predicate
        <> """ === "t" ? """
        <> serialize consequent
        <> " : "
        <> serialize alternative

    JsBool b ->
      show b

    Block xs ->
      "{" <> List.intercalate " " (serialize <$> xs) <> "}"

    JsLet name mVal ->
      case mVal of
        Nothing ->
          "let " <> name <> ";"

        Just val ->
          "let " <> name <> " = " <> serialize val <> ";"

    While cond body ->
      "while (" <> serialize cond <> ") " <> serialize body

    Not name ->
      "!" <> name

    Const name val ->
      "const " <> name <> " = " <> serialize val <> ";"

    Assignment name val ->
      name <> " = " <> serialize val <> ";"

    Return a ->
      "return " <> serialize a <> ";"

    BareReturn ->
      "return;"

    Throw e ->
      "throw " <> e

serializeCommaSeparated :: List AST -> String
serializeCommaSeparated xs =
  List.intercalate ", " (serialize <$> xs)
