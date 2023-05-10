module MidnightLang.Sexp where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List ((:))
import Data.List as List
import Data.List as PsList
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (for, traverse)
import Dodo (Doc)
import Dodo as Dodo
import Lib.Sexp as GenericSexp

type PsList = List.List

-- | The sexp type for midnight.
-- |
-- | Like 'Lib.Sexp.Sexp', this supports:
-- |
-- |   - atoms
-- |   - proper lists (as in plain `(a b c)` lists)
-- |   - `'` quotes
-- |   - `;` comments
-- |
-- | Unlike 'Lib.Sexp.Sexp':
-- |
-- |   - strings don't get their own constructor in the `Sexp` data type
-- |   - there's a specific constructor for `Int`s
-- |
-- | More on strings: they're internally represented as a tagged list of ints,
-- | but they're parsed and pretty printed using `"` bracketed syntax.
-- | So the string `abc` can be written either `"abc"` or `'(string-tag (97 98 99))`.
-- | It will always be printed as `'(string-tag (97 98 99))`.
data Sexp
  = Symbol String
  | List (PsList Sexp)
  | Int Int

derive instance Eq Sexp
derive instance Generic Sexp _
instance Show Sexp where
  show a = genericShow a

-- * Print

print :: Sexp -> String
print =
  GenericSexp.print <<< toGenericSexp

prettyprint :: forall a. Sexp -> Doc a
prettyprint =
  GenericSexp.prettyprint <<< toGenericSexp

prettyprintCols80 :: Sexp -> String
prettyprintCols80 sexp =
  Dodo.print Dodo.plainText Dodo.twoSpaces { pageWidth = 80 } (prettyprint sexp)

toGenericSexp :: Sexp -> GenericSexp.Sexp
toGenericSexp = case _ of
  Symbol s ->
    GenericSexp.Atom s

  List xs ->
    GenericSexp.List (toGenericSexp <$> xs)

  Int n ->
    GenericSexp.Atom (show n)

-- * Parse

-- | Uses 'Lib.Sexp.parse' to start, then transforms the result.
parse :: String -> Either String Sexp
parse s =
  toMidnight <$> GenericSexp.parse s
  where
  toMidnight :: GenericSexp.Sexp -> Sexp
  toMidnight =
    case _ of
      GenericSexp.Atom symbol ->
        case parseInt symbol of
          Left _ ->
            Symbol symbol

          Right i ->
            Int i

      GenericSexp.List xs ->
        List (toMidnight <$> xs)

      GenericSexp.StringLiteral str ->
        purescriptStringToMidnightString str

-- * Int

parseInt :: String -> Either String Int
parseInt s =
  case CodeUnits.uncons s of
    Nothing ->
      Left "parseInt: empty string"

    Just { head, tail } ->
      if head == '-' then
        negate <$> parsePositive tail
      else parsePositive s

  where
  parsePositive :: String -> Either String Int
  parsePositive a =
    if not (allDigits a) then
      Left "parseInt: non digit found (after possible - sign)"
    else
      case Int.fromString a of
        Nothing ->
          Left "parseInt: fromString failed"

        Just b ->
          pure b

  allDigits :: String -> Boolean
  allDigits a =
    Set.subset
      (Set.fromFoldable (CodeUnits.toCharArray a))
      ( Set.fromFoldable
          [ '0'
          , '1'
          , '2'
          , '3'
          , '4'
          , '5'
          , '6'
          , '7'
          , '8'
          , '9'
          ]
      )

-- * String

--  Sexp.List (Sexp.Symbol "string-tag" : midnightStr : PsList.Nil)
midnightStringToPureScriptString :: Sexp -> Either String String
midnightStringToPureScriptString = case _ of
  Symbol s ->
    Left ("expected a midnight string, but go symbol: " <> s)

  List xs ->
    case xs of
      Symbol "string-tag" : listInt : PsList.Nil ->
        listIntToPureScript listInt

      _ ->
        Left ("expected a midnight string, but didn't find a two-element list starting with string-tag")

  Int n ->
    Left ("expected a midnight string, but go int: " <> show n)

-- | Convert a list of Midnight ints to a PureScript `String`.
listIntToPureScript :: Sexp -> Either String String
listIntToPureScript sexp = do
  xs <- allInts sexp
  case allCodePoints xs of
    Nothing ->
      Left "not a code point"

    Just codePoints ->
      pure (String.fromCodePointArray codePoints)
  where
  allInts :: Sexp -> Either String (Array Int)
  allInts = case _ of
    Symbol _ ->
      Left "allInts"

    List xs ->
      Array.fromFoldable <$> for xs getInt

    Int _ ->
      Left "allInts"
    where
    getInt :: Sexp -> Either String Int
    getInt = case _ of
      Symbol _ ->
        Left "getInt"

      List _ ->
        Left "getInt"

      Int n ->
        pure n

  allCodePoints :: Array Int -> Maybe (Array CodePoint)
  allCodePoints =
    traverse toEnum

purescriptStringToMidnightString :: String -> Sexp
purescriptStringToMidnightString psString =
  List
    ( Symbol "quote"
        : purescriptStringToMidnightStringNoQuote psString
        : PsList.Nil
    )

purescriptStringToMidnightStringNoQuote :: String -> Sexp
purescriptStringToMidnightStringNoQuote psString =
  List
    ( Symbol "string-tag"
        : toListInt psString
        : PsList.Nil
    )
  where
  toListInt :: String -> Sexp
  toListInt str =
    List (Int <$> toCodePoints str)
    where
    toCodePoints :: String -> PsList Int
    toCodePoints s =
      toCodePointInt <$> chars
      where
      chars :: PsList CodePoint
      chars =
        Array.toUnfoldable (String.toCodePointArray s)

      toCodePointInt :: CodePoint -> Int
      toCodePointInt =
        fromEnum
