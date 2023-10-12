module Test.MidnightSystem where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.List as PsList
import Data.Maybe (Maybe(..), isJust)
import Effect.Class (liftEffect)
import Generated.EditorSource as EditorSource
import Lib.Moore as Moore
import MidnightJS as MidnightJS
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp
import MidnightSystem (Output(..), StartupFailure(..))
import MidnightSystem as MidnightSystem
import MidnightSystem.Keyboard as Keyboard
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldNotSatisfy)

replaceLetBody :: Sexp -> Sexp -> Either String Sexp
replaceLetBody newBody =
  case _ of
    Sexp.List (Sexp.Symbol "let" : varlist : _ : PsList.Nil) ->
      Right (Sexp.List (Sexp.Symbol "let" : varlist : newBody : PsList.Nil))

    _ ->
      Left "replaceLet"

spec :: Spec Unit
spec = do
  describe "generate-editor-js-file" do
    it "run" do
      js <-
        case MidnightJS.midnightToJS <$> Sexp.parse EditorSource.string of
          Left e -> do
            fail e
            pure "should-never-be-hit"
          Right a ->
            pure a

      liftEffect (writeTextFile UTF8 "editor-gitignored.js" js)
      true `shouldEqual` true

  describe "generate-macroexpanded-midnight-file" do
    it "run" do
      case Sexp.parse EditorSource.string of
        Left e ->
          fail e

        Right sexp ->
          case replaceLetBody (Sexp.Symbol "midnight-let") sexp of
            Left e ->
              fail e

            Right new ->
              case MidnightJS.eval (Sexp.print new) of
                Left e ->
                  fail e

                Right expandedMidnight ->
                  case Sexp.parse expandedMidnight of
                    Left e ->
                      fail e

                    Right expandedMidnightSexp -> do
                      liftEffect
                        ( writeTextFile
                            UTF8
                            "editor-macroexpanded-gitignored.scm"
                            expandedMidnight
                        )
                      liftEffect
                        ( writeTextFile
                            UTF8
                            "editor-macroexpanded-gitignored.js"
                            (MidnightJS.midnightToJS expandedMidnightSexp)
                        )
                      pure unit

  describe "starts-up-and-accepts-abc-input" do
    it "works" do
      case MidnightJS.evalToForeign EditorSource.string of
        Left e ->
          fail e

        Right f -> do
          let
            startingInput =
              """'(system-input-start-with-editor-contents (string-tag (97 98 99)))"""
          case MidnightJS.evalToForeign startingInput of
            Left e ->
              fail e

            Right jsVal ->
              case MidnightJS.applyClosure f [ jsVal ] of
                Left e ->
                  fail e

                Right _ ->
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
                    -- : { key: Keyboard.KeyEnter, ctrlOrMeta: true } -- restart
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

getCrash :: Output -> Maybe String
getCrash = case _ of
  OutputCrash e ->
    Just e

  OutputSuccess _ ->
    Nothing
