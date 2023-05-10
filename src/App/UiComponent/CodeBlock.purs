module App.UiComponent.CodeBlock where

import Halogen.HTML (HTML)
import Halogen.HTML as HH

html :: forall w action. String -> HTML w action
html source =
  HH.pre_
    [ HH.code_
        [ HH.text source ]
    ]
