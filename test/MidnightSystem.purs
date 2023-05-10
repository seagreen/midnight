module Test.MidnightSystem where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.List as PsList
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Debug (spy)
import Generated.EditorSource as EditorSource
import Lib.Debug (crash)
import Lib.Image (Image(..))
import Lib.Moore as Moore
import MidnightBiwa as Biwa
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp
import MidnightSystem (Output(..), StartupFailure(..))
import MidnightSystem as MidnightSystem
import MidnightSystem.Keyboard as Keyboard
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldNotSatisfy)

spec :: Spec Unit
spec = do
  describe "works" do
    it "recognizes all image constructors" do
      let
        moore =
          case MidnightSystem.moore imageTest of
            Left (StartupFailure e) ->
              crash e

            Right a ->
              a

        image =
          case (unwrap moore).output of
            OutputCrash e ->
              crash e

            OutputSuccess a ->
              a.image
      image
        `shouldEqual`
          Overlay
            ( Overlay
                ( Overlay
                    ( Overlay
                        ( Overlay
                            ( Overlay
                                Empty
                                (Line { x: -200.0, y: 200.0 } { x: 200.0, y: 200.0 })
                            )
                            (Line { x: 200.0, y: 200.0 } { x: 200.0, y: -200.0 })
                        )
                        (Line { x: 200.0, y: -200.0 } { x: -200.0, y: -200.0 })
                    )
                    (Line { x: -200.0, y: -200.0 } { x: -200.0, y: 200.0 })
                )
                (Translate { x: -100.0, y: 100.0 } (Text "ABC"))
            )
            Empty

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

                    Right expandedSexp -> do
                      -- _ <- pure (spy "macroexpanded" (Sexp.prettyprintCols80 expandedSexp))
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

imageTest :: String
imageTest =
  """
(let
  (
    (list (lambda xs
      xs))

    (example-image
      '(image-multiple
        ((image-line -200 200 200 200)
         (image-line 200 200 200 -200)
         (image-line 200 -200 -200 -200)
         (image-line -200 -200 -200 200)
         (image-string -100 100 (string-tag (65 66 67)))
         image-empty)))
  )

  (lambda (src)
    (list 'output-normal example-image 'store 'ephem)))
"""
