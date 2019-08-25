module ImageProcessing.Drawing where

import ImageProcessing.Types


pasteOver :: (Int, Int) -> Bitmap -> E Bitmap
pasteOver (x, y) bitmap overBitmap =
    let bitmapIndexed = concat $ (\ (r, l) ->
                (\ (c, p) -> ((c, r), p) ) <$> zip [x..] l
            ) <$> zip [y..] bitmap
    in foldr (uncurry putPixel) overBitmap bitmapIndexed

