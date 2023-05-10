module Lib.Debug where

import Prelude

import Partial.Unsafe as Partial.Unsafe
import Unsafe.Coerce (unsafeCoerce)

todo :: forall a. a
todo =
  unsafeCoerce unit

crash :: forall a. String -> a
crash =
  Partial.Unsafe.unsafeCrashWith
