module App.Halogen where

import Prelude

import Halogen as H

-- | From realworld-halogen:
-- |
-- | > When a component has no queries or messages,
-- | > it has no public interface and can be considered an "opaque" component.
-- | > The only way for a parent to interact with the component is by sending input.
type OpaqueSlot slot = forall query. H.Slot query Void slot
