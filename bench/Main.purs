module Bench.Main where

import Prelude

import Data.Array ((..))
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Function (applyN)
import Data.List (List, (:))
import Data.List as List
import Effect (Effect)
import Effect.Console as Console
import Generated.EditorSource as EditorSource
import Lib.Debug (crash)
import Lib.Moore as Moore
import MidnightBiwa as MidnightBiwa
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp
import MidnightSystem as MidnightSystem
import MidnightSystem.Keyboard as Keyboard
import Performance.Minibench as Minibench

main :: Effect Unit
main = do
  bench "purescript-sum" purescriptSum
  bench "midnight-sum" midnightSum
  bench "purescript-snoc" purescriptSnoc
  bench "midnight-snoc" midnightSum
  bench "editor-startup" editorStartup
  editorType10

bench :: forall a. String -> (Unit -> a) -> Effect Unit
bench label f = do
  Console.log "---------------------"
  Console.log label
  Console.log "-"
  Minibench.benchWith 5 f
  Console.log ""

-- * sum

sumNum :: Int
sumNum =
  30_000

purescriptSum :: Unit -> Int
purescriptSum _ =
  sum (1 .. sumNum)

-- | Via biwa
midnightSum :: Unit -> Int
midnightSum _ =
  case MidnightBiwa.evalToSexp midnightSumSrc of
    Left e ->
      crash e

    Right (Sexp.Int n) ->
      n

    Right _ ->
      crash "midnightSum"

attachTargetNum :: Int -> String -> String
attachTargetNum n src =
  "(let ((TARGET_NUM " <> show n <> ")) "
    <> src
    <> ")"

midnightSumSrc :: String
midnightSumSrc =
  attachTargetNum sumNum
    """
(let
  ((-
     (lambda (a b)
       (+ a (* -1 b))))
   (go
     (lambda (acc n)
       (if
         (= n 0)
         acc
         (go (+ acc n) (- n 1)))))
  )
(go 0 TARGET_NUM))
"""

sumCheck :: Unit
sumCheck =
  let
    ps = purescriptSum unit
    midnight = midnightSum unit
  in
    if ps /= midnight then
      crash
        ( "purescriptSumCheck, PS: "
            <> show ps
            <> " midnight: "
            <> show midnight
        )
    else
      unit

-- snoc

snocNum :: Int
snocNum =
  200

purescriptSnoc :: Unit -> List Int
purescriptSnoc _ =
  applyN (\xs -> List.snoc xs 1) snocNum List.Nil

-- | Via biwa
midnightSnoc :: Unit -> List Int
midnightSnoc _ =
  case MidnightBiwa.evalToSexp midnightSnocSrc of
    Left e ->
      crash e

    Right (Sexp.List xs) ->
      map toInt xs

    Right _ ->
      crash "midnightSnoc"
  where
  toInt :: Sexp -> Int
  toInt = case _ of
    Sexp.Int n ->
      n

    _ ->
      crash "toInt"

midnightSnocSrc :: String
midnightSnocSrc =
  attachTargetNum snocNum
    """
(let
  ((-
     (lambda (a b)
       (+ a (* -1 b))))
   (snoc
     (lambda (xs x)
       (if
         (list-empty? xs)
         (cons x '())
         (cons (car xs) (snoc (cdr xs) x)))))
   (go
     (lambda (acc n)
       (if
         (= n 0)
         acc
         (go (snoc acc 1) (- n 1)))))
  )
(go '() TARGET_NUM))
"""

snocCheck :: Unit
snocCheck =
  let
    ps = purescriptSnoc unit
    midnight = midnightSnoc unit
  in
    if ps /= midnight then
      crash
        ( "snocCheck, PS: "
            <> show ps
            <> " midnight: "
            <> show midnight
        )
    else
      unit

-- * editor startup

editorStartup :: Unit -> Unit
editorStartup _ =
  const unit (MidnightSystem.moore EditorSource.string)

-- * editor typing

editorType10 :: Effect Unit
editorType10 =
  case MidnightSystem.moore EditorSource.string of
    Left e ->
      Console.log ("editorType10 failed to start: " <> show e)

    Right moore ->
      bench "editor-type-10"
        -- TODO: Make a version of stepMultiple that forces checking for crashes
        ( \_ -> Moore.stepMultiple moore
            ( Keyboard.aKey
                : Keyboard.aKey
                : Keyboard.aKey
                : Keyboard.aKey
                : Keyboard.aKey
                : Keyboard.aKey
                : Keyboard.aKey
                : Keyboard.aKey
                : Keyboard.aKey
                : Keyboard.aKey
                : List.Nil
            )
        )
