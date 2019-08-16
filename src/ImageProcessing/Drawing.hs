module ImageProcessing.Drawing where

import ImageProcessing.Types
import Control.Lens



getPixel :: (Int, Int) -> Bitmap -> Color
getPixel (x, y) bitmap = case bitmap ^? ix y . ix x of
    Just p  -> p
    _       -> transparent
    

putPixel' :: (Int, Int) -> Color -> E Bitmap
putPixel' (x, y) color bitmap = bitmap & ix y . ix x .~ color


putPixel :: (Int, Int) -> Color -> E Bitmap
putPixel (x, y) color bitmap = putPixel' (x, y)
    (color <> getPixel (x, y) bitmap) bitmap


pasteOver :: (Int, Int) -> Bitmap -> E Bitmap
pasteOver (x, y) bitmap overBitmap =
    let bitmapIndexed = concat $ (\ (r, l) ->
                (\ (c, p) -> ((c, r), p) ) <$> zip [x..] l
            ) <$> zip [y..] bitmap
    in foldr (uncurry putPixel) overBitmap bitmapIndexed

