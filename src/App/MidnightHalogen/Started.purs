module App.MidnightHalogen.Started where

import Prelude

import App.MidnightHalogen.Keyboard (isRelaunchRequested, toMidnightKey)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String.CodePoints as CodePoints
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Lib.Moore (Moore(..))
import MidnightLang.Sexp as Sexp
import MidnightSystem as MidnightSystem
import MidnightSystem.Display (Display)
import MidnightSystem.Keyboard (Keyboard)
import MidnightSystem.Output (Output(..))
import MidnightSystem.StartFromStore (startFromStoreText)
import MidnightSystem.Util (foreignToSexp, foreignToSexpAllowClosures)
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
  , storeTabText :: Maybe String
  , sourceTabText :: String
  , moore :: Moore Keyboard Output
  , lastError :: Maybe String
  }

data Action
  = Init
  | SwitchMode Mode
  | HandleKey H.SubscriptionId KeyboardEvent
  | SetStore String
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
        , storeTabText: Nothing
        , sourceTabText: startingCode
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
      H.modify_ (\s -> s { storeTabText = Nothing })
      H.modify_ (\s -> s { mode = mode })

    HandleKey sid ev -> do
      mode <- H.gets _.mode
      case mode of
        Live ->
          case toMidnightKey ev of
            Nothing ->
              pure unit

            Just key -> do
              preventDefault ev
              stepKey key

        Store ->
          when (isRelaunchRequested ev) relaunchFromStore

        Source ->
          when (isRelaunchRequested ev) relaunchFromSource

        _ ->
          pure unit

    SetStore str -> do
      H.modify_ (\s -> s { storeTabText = Just str })

    SetSource str -> do
      H.modify_ (\s -> s { sourceTabText = str })

    where
    stepKey :: Keyboard -> H.HalogenM State Action slots o m Unit
    stepKey key = do
      Moore m <- H.gets _.moore
      let Moore newM = m.step key
      case newM.output of
        OutputCrash e ->
          H.modify_ (\s -> s { lastError = Just e })

        OutputSuccess _ ->
          H.modify_ (\s -> s { moore = Moore newM })

    preventDefault :: KeyboardEvent -> H.HalogenM State Action slots o m Unit
    preventDefault ev =
      H.liftEffect $ E.preventDefault (KE.toEvent ev)

    relaunchFromStore :: H.HalogenM State Action slots o m Unit
    relaunchFromStore = do
      mStr <- H.gets _.storeTabText
      case mStr of
        Nothing ->
          pure unit

        Just str ->
          case startFromStoreText str of
            Left e ->
              H.modify_ (\s -> s { lastError = Just e })

            Right moore ->
              H.modify_ (\s -> s { mode = Live, moore = moore })

    relaunchFromSource :: H.HalogenM State Action slots o m Unit
    relaunchFromSource = do
      str <- H.gets _.sourceTabText
      case MidnightSystem.startFromSource str of
        Left e ->
          H.modify_ (\s -> s { lastError = Just e })

        Right moore ->
          H.modify_ (\s -> s { mode = Live, moore = moore })

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state =
    HH.div_
      [ renderAbout
      , renderButtons state.mode
      , renderLastError state.lastError
      , case state.mode of
          Live ->
            renderLive (unwrap state.moore).output

          Display ->
            renderDisplay (unwrap state.moore).output

          Store ->
            renderStore (unwrap state.moore).output state.storeTabText

          Ephem ->
            renderEphem (unwrap state.moore).output

          Source ->
            renderSource state.sourceTabText
      ]

renderAbout :: forall slots m. H.ComponentHTML Action slots m
renderAbout =
  HH.div
    [ HP.class_ (H.ClassName "md:flex md:flex-row md:justify-center mt-1") ]
    [ HH.span_
        [ HH.text "("
        , HH.a
            [ HP.href "https://domain-j.com/Midnight-System/uuid/9885fa97-705e-4d68-a0cb-329c3e4b233e"
            , HP.class_ (H.ClassName "underline text-blue-600 hover:text-blue-800 visited:text-purple-600")
            ]
            [ HH.text "about" ]
        , HH.text ")"
        ]
    ]

renderButtons :: forall slots m. Mode -> H.ComponentHTML Action slots m
renderButtons currentMode =
  HH.div
    [ HP.class_ (H.ClassName "md:flex md:flex-row md:justify-center space-x-2 mt-3") ]
    [ button Live
    , button Display
    , button Store
    -- TODO: re-enable
    -- , button Ephem
    , button Source
    ]
  where
  button :: Mode -> H.ComponentHTML Action slots m
  button mode =
    HH.button
      ( if mode == currentMode then
          [ HP.class_ (H.ClassName "rounded-full border border-black p-2 min-w-[70px] shadow-custom-selected underline")
          -- For some reason border-2 in Tailwind wasn't working with rounded buttons
          , HP.style "border-width: 2px; cursor: revert; "
          ]
        else
          [ HE.onClick (\_ -> SwitchMode mode)
          , HP.class_ (H.ClassName "rounded-full border border-black p-2 min-w-[70px] shadow-custom hover:shadow-custom-hover")
          , HP.style "border-width: 2px;"
          ]
      )
      [ HH.text (show mode) ]

renderLastError :: forall slots m. Maybe String -> H.ComponentHTML Action slots m
renderLastError lastError =
  HH.div_
    ( case lastError of
        Nothing ->
          []

        Just e ->
          [ HH.p
              [ HP.class_ (H.ClassName "mt-5") ]
              [ HH.text e ]
          ]
    )

renderLive :: forall slots m. Output -> H.ComponentHTML Action slots m
renderLive output =
  HH.div_
    [ case output of
        OutputCrash err ->
          HH.p
            [ HP.class_ (H.ClassName "mt-5") ]
            [ HH.text err ]

        OutputSuccess { display } ->
          displayToHtml display
    ]

renderDisplay :: forall slots m. Output -> H.ComponentHTML Action slots m
renderDisplay = case _ of
  OutputCrash _ ->
    HH.p
      [ HP.class_ (H.ClassName "mt-5") ]
      [ HH.text "No display present since the app is currently crashed." ]

  OutputSuccess { displaySexp } ->
    codeBlock (Sexp.prettyprintColsPrefer80 displaySexp)

renderStore :: forall slots m. Output -> Maybe String -> H.ComponentHTML Action slots m
renderStore output mStoreTabText =
  case mStoreTabText of
    Nothing ->
      outputToStoreText output

    Just storeText ->
      renderSuccess storeText

  where
  renderSuccess :: String -> H.ComponentHTML Action slots m
  renderSuccess txt =
    HH.div
      [ HP.class_ (H.ClassName "mt-5") ]
      [ HH.p_
          [ HH.text "Ctrl-<enter> to relaunch." ]
      , HH.textarea
          [ HP.value txt
          , HP.class_ (H.ClassName "font-mono overflow-x-auto whitespace-pre min-w-[700px] h-screen p-4 bg-gray-50")
          , HE.onValueInput SetStore
          ]
      ]

  outputToStoreText :: Output -> H.ComponentHTML Action slots m
  outputToStoreText =
    case _ of
      OutputCrash err ->
        HH.p_ [ HH.text err ]

      OutputSuccess { store } ->
        case foreignToSexp store of
          Left e ->
            HH.p_ [ HH.text ("Couldn't process store: " <> e) ]

          Right sexp ->
            renderSuccess (Sexp.prettyprintColsPrefer80 sexp)

renderEphem :: forall slots m. Output -> H.ComponentHTML Action slots m
renderEphem output =
  HH.div
    [ HP.class_ (H.ClassName "mt-5") ]
    [ case output of
        OutputCrash err ->
          HH.p_ [ HH.text err ]

        OutputSuccess { ephem } ->
          case foreignToSexpAllowClosures ephem of
            Left e ->
              HH.p_ [ HH.text ("Couldn't process ephem: " <> e) ]

            Right sexp ->
              codeBlock (Sexp.prettyprintColsPrefer80 sexp)
    ]

renderSource :: forall slots m. String -> H.ComponentHTML Action slots m
renderSource sourceTabText =
  HH.div
    [ HP.class_ (H.ClassName "mt-5") ]
    [ HH.p_
        [ HH.text "Ctrl-<enter> to relaunch." ]
    , HH.textarea
        [ HP.value sourceTabText
        , HP.class_ (H.ClassName "font-mono overflow-x-auto whitespace-pre min-w-[700px] h-screen p-4 bg-gray-50")
        , HE.onValueInput SetSource
        ]
    ]

displayToHtml :: forall slots m. Display -> H.ComponentHTML Action slots m
displayToHtml display =
  HH.pre
    -- Without overflow-x-auto on mobile the display border is screen sized,
    -- but if you scroll left you see that the text overflows the border
    [ HP.class_ (H.ClassName "border-solid border-2 border-black p-5 mt-5 overflow-x-auto font-['Lekton']") ]
    linesWithCursor
  where
  linesWithCursor :: Array (H.ComponentHTML Action slots m)
  linesWithCursor =
    fold
      ( Array.mapWithIndex
          ( \i line ->
              if i + 1 == display.cursorPosition.y then
                addCursorToLine line <> [ HH.text "\n" ]
              else
                [ HH.text (line <> "\n") ]
          )
          display.text
      )

  addCursorToLine :: String -> Array (H.ComponentHTML Action slots m)
  addCursorToLine line =
    Array.mapWithIndex
      ( \i char ->
          if i + 1 == display.cursorPosition.x then
            HH.span
              [ HP.class_ (H.ClassName "underline") ]
              [ HH.text (CodePoints.singleton char) ]
          else
            HH.text (CodePoints.singleton char)
      )
      ( padArray
          display.cursorPosition.x
          (CodePoints.codePointFromChar ' ')
          (CodePoints.toCodePointArray line)
      )

padArray :: forall a. Int -> a -> Array a -> Array a
padArray n val arr =
  arr <> Array.replicate (n - Array.length arr) val

codeBlock :: forall slots m. String -> H.ComponentHTML Action slots m
codeBlock code =
  HH.pre
    -- NOTE: maybe shouldn't put the mt-5 here:
    [ HP.class_ (H.ClassName "bg-gray-100 rounded max-w-2xl overflow-x-auto mt-5") ]
    [ HH.code_
        [ HH.text code ]
    ]
