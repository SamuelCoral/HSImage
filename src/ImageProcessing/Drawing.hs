module ImageProcessing.Drawing where

import ImageProcessing.Types
import Control.Lens


mixColors :: Color -> E Color
mixColors (r1, g1, b1, a1) (r2, g2, b2, a2) =
    let a3 = a1 + a2 * (1 - a1)
        [r3, g3, b3] = (\ (c1, c2) ->
                (c1 * a1 + c2 * a2 * (1 - a1)) / a3
            ) <$> [(r1, r2), (g1, g2), (b1, b2)]
    in (r3, g3, b3, a3)


getPixel :: (Int, Int) -> Bitmap -> Color
getPixel (x, y) bitmap = case bitmap ^? ix y . ix x of
    Just p  -> p
    _       -> transparent
    

putPixel' :: (Int, Int) -> Color -> E Bitmap
putPixel' (x, y) color bitmap = bitmap & ix y . ix x .~ color


putPixel :: (Int, Int) -> Color -> E Bitmap
putPixel (x, y) color bitmap = putPixel' (x, y)
    (color `mixColors` getPixel (x, y) bitmap) bitmap


pasteOver :: (Int, Int) -> Bitmap -> E Bitmap
pasteOver (x, y) bitmap overBitmap =
    let bitmapIndexed = concat $ (\ (r, l) ->
                (\ (c, p) -> ((c, r), p) ) <$> zip [x..] l
            ) <$> zip [y..] bitmap
    in foldr (uncurry putPixel) overBitmap bitmapIndexed

