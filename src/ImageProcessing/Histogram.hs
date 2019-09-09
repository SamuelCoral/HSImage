module ImageProcessing.Histogram where

import qualified Data.Map as Map
import Control.Lens
import GHC.Word
import ImageProcessing.Types
import ImageProcessing.Geometry
import Data.Tuple


type Histogram = Map.Map Word8 Integer


count :: Ord k => [k] -> Map.Map k Integer
count = foldr (uncurry $ Map.insertWith $ const succ) Map.empty . flip zip (repeat 1)


accumSum :: Num a => [a] -> [a]
accumSum = accumSumAux 0
    where   accumSumAux ac (x:xs) =
                let s = ac + x
                in  s : accumSumAux s xs
            accumSumAux _ _ = []
            

histogram :: E Color -> Bitmap -> Histogram
histogram method = count . concat . over pixels (view red . method)


accumHistogram :: E Color -> Bitmap -> Histogram
accumHistogram method bitmap =
    let hist = histogram method bitmap
        (k, v) = accumSum <$> unzip (Map.toList hist)
    in Map.fromList $ zip k v


plotHistogram :: Color -> Histogram -> E Bitmap
plotHistogram color hist out = 
    let (w, h) = bitmapDimensions out
        scaleX = fromIntegral w / 0x100
        scaleY = fromIntegral h / fromIntegral (maximum hist)
        bar = take h $ groupsOf (floor scaleX - 1) $ repeat color
    in  Map.foldrWithKey (\ i n -> pasteOver (
                round $ fromIntegral i * scaleX,
                h - round (fromIntegral n * scaleY)
            ) bar) out hist


illuminationLevel :: Histogram -> Float
illuminationLevel hist =
    fromIntegral (sum $ uncurry (*) . fmap fromIntegral . swap <$> Map.toList hist) /
    fromIntegral (sum hist)


contrastLevel :: Histogram -> Word8
contrastLevel hist =
    let h = [ x | x <- Map.keys hist, x /= 0 ]
    in  maximum h - minimum h
