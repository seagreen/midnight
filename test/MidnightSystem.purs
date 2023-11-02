module Test.MidnightSystem where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.List as PsList
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import EditorHuge as EditorHuge
import Effect.Class (liftEffect)
import Generated.EditorSource as EditorSource
import Generated.HelloWorldSource as HelloWorldSource
import Lib.Moore as Moore
import MidnightJS as MidnightJS
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp
import MidnightSystem as MidnightSystem
import MidnightSystem.Keyboard as Keyboard
import MidnightSystem.Output (Output(..))
import MidnightSystem.StartFromStore (startFromStoreText)
import MidnightSystem.Util (foreignToSexp)
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

    it "hello world example works" do
      case MidnightSystem.startFromSource HelloWorldSource.string of
        Left e ->
          fail e

        Right _ ->
          pure unit

    it "initial editor outputs an image successfully" do
      case MidnightSystem.startFromSource EditorSource.string of
        Left e ->
          fail e

        Right _ ->
          pure unit

    it "editor accepts some commands" do
      case MidnightSystem.startFromSource EditorSource.string of
        Left e ->
          fail e

        Right moore ->
          let
            outputs =
              -- TODO: would be better to stop after the first failure
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

    it "performance check - huge editor with with tons of definitions" do
      case MidnightSystem.startFromSource (EditorHuge.string 1000) of
        Left e ->
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

    it "editor starts from store" do
      case MidnightSystem.startFromSource EditorSource.string of
        Left e ->
          fail ("StartupFailure: " <> e)

        Right startingMoore ->
          case (unwrap startingMoore).output of
            OutputCrash e ->
              fail ("OutputCrash in startingMoore: " <> e)

            OutputSuccess output ->
              case foreignToSexp output.store of
                Left e ->
                  fail ("foreignToSexp failure: " <> e)

                Right storeSexp ->
                  case startFromStoreText (Sexp.prettyprintColsPrefer80 storeSexp) of
                    Left e ->
                      fail ("startFromStoreText failure: " <> e)

                    Right moore ->
                      let
                        outputs =
                          Moore.stepMultipleUnlessPred moore getCrash
                            ( Keyboard.noMeta (Keyboard.KeyArrow Keyboard.ArrowDown)
                                : { key: Keyboard.KeyEnter, ctrlOrMeta: true } -- restart
                                : Keyboard.noMeta (Keyboard.KeyArrow Keyboard.ArrowDown)
                                : PsList.Nil
                            )
                      in
                        outputs `shouldNotSatisfy` isJust

    -- NOTE: almost full duplication between this and above test case
    it "performance - editor starts from store with huge editor" do
      case MidnightSystem.startFromSource (EditorHuge.string 300) of
        Left e ->
          fail ("StartupFailure: " <> e)

        Right startingMoore ->
          case (unwrap startingMoore).output of
            OutputCrash e ->
              fail ("OutputCrash in startingMoore: " <> e)

            OutputSuccess output ->
              case foreignToSexp output.store of
                Left e ->
                  fail ("foreignToSexp failure: " <> e)

                Right storeSexp ->
                  case startFromStoreText (Sexp.prettyprintColsPrefer80 storeSexp) of
                    Left e ->
                      fail ("startFromStoreText failure: " <> e)

                    Right moore ->
                      let
                        outputs =
                          Moore.stepMultipleUnlessPred moore getCrash
                            ( Keyboard.noMeta (Keyboard.KeyArrow Keyboard.ArrowDown)
                                : { key: Keyboard.KeyEnter, ctrlOrMeta: true } -- restart
                                : Keyboard.noMeta (Keyboard.KeyArrow Keyboard.ArrowDown)
                                : PsList.Nil
                            )
                      in
                        outputs `shouldNotSatisfy` isJust

{-
-- Disabled because it's slow.
--
-- NOTE: At 5000 it stack overflows.
it "lots-o-restarts" do
  case MidnightSystem.startFromSource EditorSource.string of
    Left e ->
      fail e

    Right moore ->
      let
        listReplicate n x = List.fromFoldable (Array.replicate n x)

        outputs =
          Moore.stepMultipleUnlessPred moore getCrash
            (listReplicate 3000 { key: Keyboard.KeyChar 'a', ctrlOrMeta: true })
      in
        outputs `shouldNotSatisfy` isJust
-}

getCrash :: Output -> Maybe String
getCrash = case _ of
  OutputCrash e ->
    Just e

  OutputSuccess _ ->
    Nothing
