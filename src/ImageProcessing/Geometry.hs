{-# LANGUAGE TemplateHaskell #-}
module ImageProcessing.Geometry where

import ImageProcessing.Types
import Control.Lens


data Matrix = Matrix {
    _x1 :: Float,
    _x2 :: Float,
    _y1 :: Float,
    _y2 :: Float
} deriving (Show, Read, Eq, Ord)

$(makeLenses ''Matrix)


instance Num Matrix where
    (Matrix a b c d) + (Matrix e f g h) = Matrix (a + e) (b + f) (c + g) (d + h)
    (Matrix a b c d) - (Matrix e f g h) = Matrix (a - e) (b - f) (c - g) (d - h)
    (Matrix a b c d) * (Matrix e f g h) = Matrix
        (a * e + b * g) (a * f + b * h)
        (c * e + d * g) (c * f + d * h)
    abs (Matrix a b c d) =
        let det = a * c - b * d
        in Matrix det 0 det 0
    signum (Matrix a b c d) =
        let det = view x1 $ abs $ Matrix a b c d
        in Matrix (a / det) (b / det) (c / det) (d / det)
    fromInteger n = Matrix (fromInteger n) 0 (fromInteger n) 0


pasteOver :: (Int, Int) -> Bitmap -> E Bitmap
pasteOver (x, y) bitmap overBitmap =
    let bitmapIndexed = concat $ (\ (r, l) ->
                (\ (c, p) -> ((c, r), p) ) <$> zip [x..] l
            ) <$> zip [y..] bitmap
    in foldr (uncurry putPixel) overBitmap bitmapIndexed


--transform :: (Int, Int) -> 