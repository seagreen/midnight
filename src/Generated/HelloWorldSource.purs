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
  (
    (list (lambda xs
      xs))

    (image-multiple (lambda images
      (list 'image-multiple images)))

    (border
      (image-multiple
        '(image-line -200 200 200 200)
        '(image-line 200 200 200 -200)
        '(image-line 200 -200 -200 -200)
        '(image-line -200 -200 -200 200)))

    (example-image
      (image-multiple
        border
        (list 'image-string -100 100 "hello world")))
  )

  (lambda (system-input)
    (list 'output-normal example-image 'store 'ephem)))
"""
