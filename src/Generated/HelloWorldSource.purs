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
       (list 'cursor-position 1 1)
       (list 'text (list "hello-world"))))

   (alist-singleton
     (lambda (k v)
       (cons (list k v) '()))))

  (lambda (_system-input)
    (list
      'output-store-and-ephem
      (alist-singleton 'display example-display)
      'ephem)))
"""
