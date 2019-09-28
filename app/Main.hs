module Main where

import ImageProcessing.BMP
import ImageProcessing.Types
import ImageProcessing.Filters
import ImageProcessing.Color
import System.Random
import Control.Monad
import Control.Lens


main :: IO ()
main = do
    
    img <- readBMP "stonks.bmp"
    let (w, h) = bitmapDimensions img
        randCoord d = map (round . (* fromIntegral d)) <$>
            replicateM 100 (randomIO :: IO Double)

    wx <- randCoord w
    wy <- randCoord h
    let ws = zip wx wy

    bx <- randCoord w
    by <- randCoord h
    let bs = zip bx by
    
        noisy = foldr ($) (img & pixels %~ colorAverage) $
            (flip putPixel (RGBA 0x00 0x00 0x00 1) <$> bs) ++
            (flip putPixel (RGBA 0xFF 0xFF 0xFF 1) <$> ws)
    
    saveBMP "noisy.bmp" noisy
    saveBMP "filterednl.bmp" $ nonLinearFilter 5 maximum noisy
