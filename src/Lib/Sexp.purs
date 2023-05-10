module Lib.Sexp where

import Prelude

import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List ((:))
import Data.List as PsList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Dodo (Doc)
import Dodo as Dodo
import Parsing (runParser)
import Parsing as Parsing
import Parsing.Combinators as Combinators
import Parsing.String as ParseString

-- | A generic sexp type.
-- |
-- | Supports atoms and proper lists (as in plain `(a b c)` lists).
-- |
-- | Also features `'` quotes, `;` comments, and `""` strings.
data Sexp
  = Atom String
  | List (PsList Sexp)
  | StringLiteral String

derive instance Eq Sexp
derive instance Generic Sexp _
instance Show Sexp where
  show a = genericShow a

type PsList = PsList.List

-- * Syntax

type Syntax =
  { openParen :: Char
  , closeParen :: Char
  , whitespace :: Whitespace
  , escape :: Char
  , quote :: Char
  , comment :: Char
  , stringBracketDelimiter :: Char
  }

type Whitespace =
  { space :: Char
  , newline :: Char
  }

syn :: Syntax
syn =
  { openParen: '('
  , closeParen: ')'
  , whitespace: whitespace
  , escape: '\\'
  , quote: '\''
  , comment: ';'
  , stringBracketDelimiter: '"'
  }

whitespace :: Whitespace
whitespace =
  { space: ' '
  , newline: '\n'
  }

whitespaceChars :: Set Char
whitespaceChars =
  case whitespace of
    { space, newline } ->
      Set.fromFoldable [ space, newline ]

isWhitespace :: Char -> Boolean
isWhitespace c =
  Set.member c whitespaceChars

charsUsedBySyntax :: Set Char
charsUsedBySyntax =
  case syn of
    { openParen, closeParen, escape, comment, stringBracketDelimiter } ->
      Set.fromFoldable
        [ openParen, closeParen, escape, comment, stringBracketDelimiter ]
        <> whitespaceChars

isUsedBySyntax :: Char -> Boolean
isUsedBySyntax c =
  Set.member c charsUsedBySyntax

stringEscapeMap :: Map Char Char
stringEscapeMap =
  Map.fromFoldable
    [ Tuple whitespace.newline 'n'
    , Tuple syn.stringBracketDelimiter syn.stringBracketDelimiter
    , Tuple syn.escape syn.escape
    ]

stringEscapeMapFlipped :: Map Char Char
stringEscapeMapFlipped =
  flipMap stringEscapeMap
  where
  flipMap :: forall k v. Ord k => Ord v => Map k v -> Map v k
  flipMap m =
    Map.fromFoldable (Tuple.swap <$> (Map.toUnfoldable m :: PsList (Tuple k v)))

isStringEscapable :: Char -> Boolean
isStringEscapable c =
  Map.member c stringEscapeMap

-- * Print

print :: Sexp -> String
print = case _ of
  Atom a ->
    printAtom a

  -- NOTE: oh boy
  List (Atom "quote" : x : PsList.Nil) ->
    "'" <> print x

  List xs ->
    printList xs

  StringLiteral str ->
    printString str

-- * Prettyprint

prettyprint :: forall a. Sexp -> Doc a
prettyprint = case _ of
  Atom a ->
    prettyprintAtom a

  List xs ->
    prettyprintList xs

  StringLiteral str ->
    prettyprintString str

-- * Parse

type Parser = Parsing.Parser String

parse :: String -> Either String Sexp
parse input =
  -- Was parseErrorMessage instead of show, but that didn't give position info:
  lmap show
    (runParser input (whitespaceOrCommentP *> possiblyQuotedSexpP <* ParseString.eof))

possiblyQuotedSexpP :: Parser Sexp
possiblyQuotedSexpP =
  -- https://github.com/Thimoteus/SandScript/wiki/2.-Parsing-recursively
  fix
    ( \p ->
        quoted p <|> sexpP p
          <* whitespaceOrCommentP
    )
  where
  quoted :: Parser Sexp -> Parser Sexp
  quoted p = do
    _ <- ParseString.char syn.quote
    (\expr -> List (Atom "quote" : expr : PsList.Nil)) <$> p

sexpP :: Parser Sexp -> Parser Sexp
sexpP p =
  label "atom" (Atom <$> atomP)
    <|> label "string" (StringLiteral <$> stringLiteralP)
    -- Put list last so we see the parse errors from it:
    <|> label "list" (List <$> listP p)

-- * List

printList :: PsList Sexp -> String
printList xs =
  charToString syn.openParen
    <>
      ( String.joinWith (charToString whitespace.space) (PsList.toUnfoldable (print <$> xs))
          <> charToString syn.closeParen
      )

prettyprintList :: forall a. PsList Sexp -> Doc a
prettyprintList xs =
  Dodo.flexGroup
    ( Dodo.text (charToString syn.openParen)
        <>
          ( Dodo.indent (Dodo.foldWithSeparator Dodo.spaceBreak (prettyprint <$> xs))
              <> Dodo.text (charToString syn.closeParen)
          )
    )

listP :: Parser Sexp -> Parser (PsList Sexp)
listP p =
  Combinators.between
    (ParseString.char syn.openParen *> whitespaceOrCommentP)
    (ParseString.char syn.closeParen)
    (Combinators.many p)

-- * Atom

printAtom :: String -> String
printAtom a =
  escapeString a

prettyprintAtom :: forall a. String -> Doc a
prettyprintAtom a =
  Dodo.text a

atomP :: Parser String
atomP =
  CodeUnits.fromCharArray <$> Array.some (ParseString.satisfy (not <<< isUsedBySyntax))

-- * String

printString :: String -> String
printString str =
  delimit <> (escapeString str <> delimit)
  where
  delimit :: String
  delimit =
    charToString syn.stringBracketDelimiter

prettyprintString :: forall a. String -> Doc a
prettyprintString str =
  delimit <> (Dodo.text (escapeString str) <> delimit)
  where
  delimit :: Doc a
  delimit =
    Dodo.text (charToString syn.stringBracketDelimiter)

escapeString :: String -> String
escapeString str =
  stringFlatMap
    ( \c ->
        case Map.lookup c stringEscapeMap of
          Just v ->
            [ syn.escape, v ]

          Nothing ->
            [ c ]
    )
    str
  where
  stringFlatMap :: (Char -> Array Char) -> String -> String
  stringFlatMap f s =
    CodeUnits.fromCharArray (Array.concatMap f (CodeUnits.toCharArray s))

stringLiteralP :: Parser String
stringLiteralP =
  Combinators.between
    (ParseString.char syn.stringBracketDelimiter)
    (ParseString.char syn.stringBracketDelimiter)
    (CodeUnits.fromCharArray <$> Array.many charP)
  where
  charP :: Parser Char
  charP =
    ( ParseString.char syn.escape *>
        ParseString.anyChar >>= \c ->
        case Map.lookup c stringEscapeMapFlipped of
          Just v ->
            pure v

          Nothing ->
            Parsing.fail
              ( "escape in string literal followed by non-escapable char: "
                  <> charToString c
              )
    ) <|> ParseString.satisfy (not <<< isStringEscapable)

-- * Whitespace

whitespaceOrCommentP :: Parser Unit
whitespaceOrCommentP =
  Combinators.skipMany (whitecharP <|> commentP)
  where
  whitecharP :: Parser Unit
  whitecharP =
    ParseString.satisfy isWhitespace $> unit

  commentP :: Parser Unit
  commentP =
    ParseString.char syn.comment
      *> Combinators.skipMany (ParseString.satisfy (\c -> c /= syn.whitespace.newline))
      *> Combinators.optional (ParseString.char syn.whitespace.newline)

-- * Misc

label :: String -> Parser Sexp -> Parser Sexp
label s =
  Parsing.region
    ( \(Parsing.ParseError e position) ->
        Parsing.ParseError ("inside " <> s <> ": " <> e) position
    )

charToString :: Char -> String
charToString =
  CodeUnits.singleton
