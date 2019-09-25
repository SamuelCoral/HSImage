module ImageProcessing.Filters where

import Control.Comonad
import Data.List
import Control.Lens
import ImageProcessing.Types
import ImageProcessing.Color


instance Comonad [] where
    extract = head
    duplicate = init . tails


conv :: [[Float]] -> E Bitmap
m `conv` b =
    let b' = b & pixels %~ fromIntegral . view red . colorAverage
        (w, h) = (length $ head m, length m) & both %~ (`div` 2)
    in [] --zipWith (zipWith (*)) m' b'

    
