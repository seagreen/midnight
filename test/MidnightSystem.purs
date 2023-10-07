module Test.MidnightSystem where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.List as PsList
import Data.Maybe (Maybe(..), isJust)
import Generated.EditorSource as EditorSource
import Lib.Moore as Moore
import MidnightBiwa as Biwa
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp
import MidnightSystem (Output(..), StartupFailure(..))
import MidnightSystem as MidnightSystem
import MidnightSystem.Keyboard as Keyboard
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldNotSatisfy)

spec :: Spec Unit
spec = do
  describe "works" do
    it "macroexpands" do
      case Sexp.parse EditorSource.string of
        Left e ->
          fail e

        Right sexp ->
          case replaceLetBody (Sexp.Symbol "midnight-let") sexp of
            Left e ->
              fail e

            Right new ->
              case Biwa.eval (Sexp.print new) of
                Left e ->
                  fail e

                Right expanded ->
                  case Sexp.parse expanded of
                    Left e ->
                      fail e

                    Right _expandedSexp -> do
                      -- _ <- pure (spy "macroexpanded" (Sexp.prettyprintColsPrefer80 expandedSexp))
                      pure unit

    it "initial editor outputs an image successfully" do
      case MidnightSystem.moore EditorSource.string of
        Left (StartupFailure e) ->
          fail e

        Right _ ->
          pure unit

    it "editor accepts some commands" do
      case MidnightSystem.moore EditorSource.string of
        Left (StartupFailure e) ->
          fail e

        Right moore ->
          let
            outputs =
              Moore.stepMultipleUnlessPred moore getCrash
                ( Keyboard.noMeta (Keyboard.KeyArrow Keyboard.ArrowUp)
                    : Keyboard.noMeta (Keyboard.KeyArrow Keyboard.ArrowDown)
                    : Keyboard.noMeta (Keyboard.KeyArrow Keyboard.ArrowLeft)
                    : Keyboard.noMeta (Keyboard.KeyArrow Keyboard.ArrowRight)
                    : { key: Keyboard.KeyEnter, ctrlOrMeta: true } -- restart
                    : Keyboard.aKey
                    : Keyboard.aKey
                    : Keyboard.aKey
                    : { key: Keyboard.KeyChar 'a', ctrlOrMeta: true }
                    : Keyboard.noMeta (Keyboard.KeyEnter)
                    : Keyboard.noMeta (Keyboard.KeyBackspace)
                    : PsList.Nil
                )
          in
            outputs `shouldNotSatisfy` isJust

replaceLetBody :: Sexp -> Sexp -> Either String Sexp
replaceLetBody newBody =
  case _ of
    Sexp.List (Sexp.Symbol "let" : varlist : _ : PsList.Nil) ->
      Right (Sexp.List (Sexp.Symbol "let" : varlist : newBody : PsList.Nil))

    _ ->
      Left "replaceLet"

getCrash :: Output -> Maybe String
getCrash = case _ of
  OutputCrash e ->
    Just e

  OutputSuccess _ ->
    Nothing
