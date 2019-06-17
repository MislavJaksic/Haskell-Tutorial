module Luhn
  ( validate
  ) where

import Luhn.Internal



validate :: Int -> Bool
validate x = lastDigitZero $ sumDigits $ doubleEveryOther $ toDigits x
