module App.MidnightHalogen where

import Prelude

import App.Halogen (OpaqueSlot)
import App.MidnightHalogenStarted as App.MidnightHalogenStarted
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Lib.Moore (Moore)
import MidnightSystem (Output, StartupFailure(..))
import MidnightSystem as MidnightSystem
import MidnightSystem.Keyboard (Keyboard)
import Type.Proxy (Proxy(..))

data State
  = Unstarted
  | Starting
  | StartupError String
  | Running (Moore Keyboard Output)

data Action = Init

type ChildSlots =
  ( running :: OpaqueSlot Unit
  )

component
  :: forall q i o m
   . MonadAff m
  => String
  -> String
  -> H.Component q i o m
component name startingCode =
  H.mkComponent
    { initialState: \_ -> Starting
    , render: render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    -- Init runs before any rendering is done
    Init -> do
      -- Having separate `Starting` and `Unstarted`,
      -- along with the `delay` below,
      -- is necessary to get the loading state to show before
      -- the midnight app loads.
      --
      -- See here: https://www.reddit.com/r/purescript/comments/o3j0d1/halogen_how_to_force_rerender_after_modifying/
      --
      -- TODO: Not working if this is the first page loaded.
      H.put Starting
      H.liftAff $ delay $ Milliseconds 0.0
      _ <-
        -- Be careful with `H.fork` as it can clobber state.
        H.fork $ case MidnightSystem.moore startingCode of
          Left (StartupFailure e) ->
            H.put (StartupError e)

          Right moore ->
            H.put (Running moore)
      pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.h2_ [ HH.text name ]
      , case state of
          Unstarted ->
            HH.p_ [ HH.text "Unstarted" ] -- This should never show

          Starting ->
            HH.p_ [ HH.text "Starting" ]

          StartupError e ->
            HH.p_ [ HH.text ("Error at startup: " <> e) ]

          Running moore ->
            HH.slot_ (Proxy :: _ "running") unit (App.MidnightHalogenStarted.component startingCode moore) unit
      ]
