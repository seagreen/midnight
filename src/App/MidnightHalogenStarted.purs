module App.MidnightHalogenStarted where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Lib.ImageSvg as ImageSvg
import Lib.Moore (Moore(..))
import MidnightLang.Sexp as Sexp
import MidnightSystem (Output(..), StartupFailure(..))
import MidnightSystem as MidnightSystem
import MidnightSystem.Keyboard (ArrowKey(..), Key(..), Keyboard)
import MidnightSystem.Output as Output
import Web.Event.Event as E
import Web.HTML as Web.Html
import Web.HTML.HTMLDocument as HtmlDocument
import Web.HTML.Window as Web.Html.Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

data Mode
  = Live
  | Display
  | Store
  | Ephem
  | Source

derive instance Eq Mode
derive instance Generic Mode _
instance Show Mode where
  show a = genericShow a

type State =
  { mode :: Mode
  -- TODO: currentlyRunning is a lie, this can be changed
  -- in the textarea:
  , currentlyRunning :: String
  , moore :: Moore Keyboard Output
  , lastError :: Maybe String
  }

data Action
  = Init
  | SwitchMode Mode
  | HandleKey H.SubscriptionId KeyboardEvent
  | SetSource String

-- Key subscription strategy from here:
-- https://github.com/purescript-halogen/purescript-halogen/blob/847659d8b057ba03a992ced5b3e25e27a1e265ff/examples/keyboard-input/src/Main.purs

component
  :: forall q i o m
   . MonadEffect m
  => String
  -> Moore Keyboard Output
  -> H.Component q i o m
component startingCode startingMoore =
  H.mkComponent
    { initialState: \_ ->
        { mode: Live
        , currentlyRunning: startingCode
        , moore: startingMoore
        , lastError: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }
  where
  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Init -> do
      document <- H.liftEffect $ Web.Html.Window.document =<< Web.Html.window
      H.subscribe' \sid ->
        eventListener
          KET.keydown
          (HtmlDocument.toEventTarget document)
          (map (HandleKey sid) <<< KE.fromEvent)

    SwitchMode mode -> do
      H.modify_ (\s -> s { mode = mode })

    HandleKey sid ev -> do
      mode <- H.gets _.mode
      case mode of
        Live ->
          handleLiveKey ev

        Source ->
          handleSourceKey ev

        _ ->
          pure unit

    SetSource str -> do
      H.modify_ (\s -> s { currentlyRunning = str })

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { mode, currentlyRunning, moore, lastError } =
    HH.div_
      [ renderButtons mode
      , renderLastError lastError
      , case mode of
          Live ->
            renderLive (unwrap moore).output

          Display ->
            renderDisplay (unwrap moore).output

          Store ->
            renderStore (unwrap moore).output

          Ephem ->
            renderEphem (unwrap moore).output

          Source ->
            renderSource currentlyRunning
      ]

  renderLive :: forall slots. Output -> H.ComponentHTML Action slots m
  renderLive output =
    HH.div_
      [ case output of
          OutputCrash err ->
            HH.p_ [ HH.text err ]

          OutputSuccess { image } ->
            ImageSvg.toHalogenSvg ImageSvg.noEvent image
      ]

  renderDisplay :: forall slots. Output -> H.ComponentHTML Action slots m
  renderDisplay = case _ of
    OutputCrash _ ->
      HH.p_
        [ HH.text "No image present since the app is currently crashed." ]

    OutputSuccess { imageSexp } ->
      HH.textarea
        [ HP.value (Sexp.prettyprintColsPrefer80 imageSexp)
        , HP.disabled true
        ]

  renderStore :: forall slots. Output -> H.ComponentHTML Action slots m
  renderStore output =
    HH.div_
      [ case output of
          OutputCrash err ->
            HH.p_ [ HH.text err ]

          OutputSuccess { store } ->
            case Output.foreignToSexp store of
              Left e ->
                HH.p_ [ HH.text ("Couldn't process store: " <> e) ]

              Right sexp ->
                HH.textarea
                  [ HP.value (Sexp.prettyprintColsPrefer80 sexp)
                  , HP.disabled true
                  ]
      ]

  renderEphem :: forall slots. Output -> H.ComponentHTML Action slots m
  renderEphem output =
    HH.div_
      [ case output of
          OutputCrash err ->
            HH.p_ [ HH.text err ]

          OutputSuccess { ephem } ->
            case Output.foreignToSexpAllowClosures ephem of
              Left e ->
                HH.p_ [ HH.text ("Couldn't process ephem: " <> e) ]

              Right sexp ->
                HH.textarea
                  [ HP.value (Sexp.prettyprintColsPrefer80 sexp)
                  , HP.disabled true
                  ]
      ]

  renderSource :: forall slots. String -> H.ComponentHTML Action slots m
  renderSource currentlyRunning =
    HH.div_
      [ HH.p_
          [ HH.text "Ctrl-<enter> to relaunch." ]
      , HH.textarea
          [ HP.value currentlyRunning
          , HE.onValueInput SetSource
          ]
      ]

  renderButtons :: forall slots. Mode -> H.ComponentHTML Action slots m
  renderButtons currentMode =
    HH.div
      [ HP.style "margin-bottom: 1em" ]
      [ button Live
      , button Display
      , button Store
      , button Ephem
      , button Source
      ]
    where
    button :: Mode -> H.ComponentHTML Action slots m
    button mode =
      HH.button
        ( if mode == currentMode then
            [ HP.class_ (H.ClassName "btn btn-primary btn-ghost")
            , HP.style "margin-right: 0.5em; cursor: revert"
            ]
          else
            [ HE.onClick (\_ -> SwitchMode mode)
            , HP.class_ (H.ClassName "btn btn-default btn-ghost")
            , HP.style "margin-right: 0.5em"
            ]
        )
        [ HH.text (show mode) ]

  renderLastError :: forall slots. Maybe String -> H.ComponentHTML Action slots m
  renderLastError lastError =
    HH.div_
      ( case lastError of
          Nothing ->
            []

          Just e ->
            [ HH.p_
                [ HH.text e ]
            ]
      )

handleLiveKey
  :: forall slots o m
   . MonadEffect m
  => KeyboardEvent
  -> H.HalogenM State Action slots o m Unit
handleLiveKey ev = do
  case CodeUnits.uncons charStr of
    Nothing ->
      pure unit

    Just { head } ->
      if String.length charStr == 1 && intercept then
        step (KeyChar head)
      else
        case charStr of
          "Enter" ->
            step KeyEnter

          "Backspace" -> do
            step KeyBackspace

          "ArrowUp" -> do
            step (KeyArrow ArrowUp)

          "ArrowDown" -> do
            step (KeyArrow ArrowDown)

          "ArrowLeft" -> do
            step (KeyArrow ArrowLeft)

          "ArrowRight" -> do
            step (KeyArrow ArrowRight)

          _ ->
            pure unit
  where
  step :: Key -> H.HalogenM State Action slots o m Unit
  step key = do
    preventDefault
    Moore m <- H.gets _.moore
    let Moore newM = m.step { key, ctrlOrMeta }
    case newM.output of
      OutputCrash e ->
        H.modify_ (\s -> s { lastError = Just e })

      OutputSuccess _ ->
        H.modify_ (\s -> s { moore = Moore newM })

  -- Eg we don't intercept ctrl-r
  intercept :: Boolean
  intercept =
    not (KE.ctrlKey ev && KE.code ev == "KeyR")

  charStr :: String
  charStr =
    KE.key ev

  ctrlOrMeta :: Boolean
  ctrlOrMeta =
    KE.ctrlKey ev || KE.metaKey ev

  preventDefault :: H.HalogenM State Action slots o m Unit
  preventDefault =
    H.liftEffect $ E.preventDefault (KE.toEvent ev)

handleSourceKey
  :: forall slots o m
   . MonadEffect m
  => KeyboardEvent
  -> H.HalogenM State Action slots o m Unit
handleSourceKey ev = do
  when (charStr == "Enter" && ctrlOrMeta) relaunchFromSource
  where
  charStr :: String
  charStr =
    KE.key ev

  ctrlOrMeta :: Boolean
  ctrlOrMeta =
    KE.ctrlKey ev || KE.metaKey ev

relaunchFromSource
  :: forall slots o m. MonadEffect m => H.HalogenM State Action slots o m Unit
relaunchFromSource = do
  str <- H.gets _.currentlyRunning
  case MidnightSystem.moore str of
    Left (StartupFailure e) ->
      H.modify_ (\s -> s { lastError = Just e })

    Right moore ->
      H.modify_ (\s -> s { mode = Live, moore = moore })
