module MidnightJS.Imp where

import Prelude

import Data.Enum (fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..))

type Id = String

-- h/t PureScript
data Imp
  = Var Id
  | Lam (List String) Imp
  | LamVariadic String Imp
  | -- | immediately invoked function expression (IIFE)
    --
    -- Used to simulate lisp-style let.
    --
    -- `(() => <imp>)()`
    LamUnitImmediateInvoked Imp
  | Let (List (Tuple String Imp)) Imp
  | App Imp (List Imp)
  | If Imp Imp Imp
  | Array (List Imp)
  -- | IndexArray Imp Int
  | JSInt Int
  | JSString String
  | Block (List Imp)
  | Assignment String Imp
  | Return Imp
  | Throw Imp

derive instance Eq Imp
derive instance Generic Imp _
instance Show Imp where
  show a = genericShow a

serialize :: Imp -> String
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

    LamUnitImmediateInvoked imp ->
      "(() => " <> serialize imp <> ")()"

    Let bindingList body ->
      serialize
        ( LamUnitImmediateInvoked
            ( Block
                ( ( (\(Tuple name val) -> Assignment name val)
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

    Block xs ->
      "{" <> List.intercalate " " (serialize <$> xs) <> "}"

    Assignment name val ->
      "const " <> name <> " = " <> serialize val <> ";"

    Return a ->
      "return " <> serialize a <> ";"

    Throw e ->
      "throw " <> serialize e

serializeCommaSeparated :: List Imp -> String
serializeCommaSeparated xs =
  List.intercalate ", " (serialize <$> xs)

{-

serializeTop :: Imp -> Text
serializeTop Imp =
  case Imp of
    Block xs ->
      Text.intercalate "\n" (fmap serialize xs)
    _ ->
      serialize Imp


serialize :: Imp -> Text -- PERFORMANCE: could use a builder
serialize topImp =
  case topImp of
    Assignment a1 a2 ->
      "const " <> serialize a1 <> " = " <> serialize a2 <> ";"

    Block Imps ->
      Text.intercalate "\n" (fmap serialize Imps)

    Return Imp ->
      "return " <> serialize Imp

    IndexArray Imp index ->
      serialize Imp <> "[" <> show index <> "]"

    IfThen a1 a2 ->
      "if (" <> serialize a1 <> ") { " <> serialize a2 <> "}"

    Else Imp ->
      " else { " <> serialize Imp <> " }"

    Throw Imp ->
      "throw " <> serialize Imp

    Equal a1 a2 ->
      serialize a1 <> " === " <> serialize a2

    LambdaUnit Imp ->
      "(() => { " <> serialize Imp <> "})()"

    Compare Imp1 Imp2 ->
      "$compareBuiltin(" <> serialize Imp1 <> ", " <> serialize Imp2 <> ")"

    ShowInt Imp ->
      "$unicodeListizeBuiltin(" <> serialize Imp <> ".toString())"

serializeId :: Id -> Text
serializeId (Id t) =
  "_" <> t

-}
