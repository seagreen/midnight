module Generated.HelloWorldSource where

import Prelude

-- | Don't edit me directly!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- |
-- |
-- |
-- |
-- |
-- |
-- |
-- |
string :: String
string =
  """(let
  ((list
    (lambda xs
      xs))

  (example-display
    (list
      (list 'cursor-position 0 0)
      (list 'text (list "hello-world")))))

  (lambda (system-input)
    (list 'output-normal example-display 'store 'ephem)))
"""
