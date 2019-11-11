{-# LANGUAGE RankNTypes #-}
module ImageProcessing.Histogram where

import qualified Data.Map as Map
import Control.Lens
import GHC.Word
import ImageProcessing.Types
import ImageProcessing.Geometry
import ImageProcessing.Color
import Data.Tuple
import Data.List


type Histogram = Map.Map Word8 Integer
type HistogramF = Map.Map Word8 Float


count :: Ord k => [k] -> Map.Map k Integer
count = foldr (uncurry $ Map.insertWith $ const succ) Map.empty . flip zip (repeat 1)


histogram :: E Color -> Bitmap -> Histogram
histogram method = count . concat . over pixels (view red . method)


accumHistogram :: E [Integer] -> E Color -> Bitmap -> Histogram
accumHistogram accumulator method bitmap =
    let hist = histogram method bitmap
        (k, v) = accumulator <$> unzip (Map.toList hist)
    in Map.fromList $ zip k v


accumHistMeans :: (forall a. Num a => E [a]) -> E Color -> Bitmap -> HistogramF
accumHistMeans accumulator method bitmap =
    let hist = fromIntegral <$> histogram method bitmap
        accumHist = fromIntegral <$> accumHistogram accumulator method bitmap
        pondHist = Map.fromList $ uncurry zip $ fmap accumulator $ unzip $ Map.toList $
            Map.mapWithKey (\ i h -> fromIntegral i * h) hist
    in Map.intersectionWith (/) pondHist accumHist


accumHistVar :: (forall a. ([a] -> [[a]])) -> (forall a. Num a => E [a]) -> E Color -> Bitmap -> HistogramF
accumHistVar duplicator accumulator method bitmap = 
    let hist = fromIntegral <$> histogram method bitmap
        accumHist = fromIntegral <$> accumHistogram accumulator method bitmap
        accumHistM = accumHistMeans accumulator method bitmap
        dupHist = Map.fromList $ zip (Map.keys hist) $ duplicator $ Map.toList hist
        pondHist = Map.intersectionWith (\ hs m ->
            sum $ (\ (i, h) -> (fromIntegral i - m)^2 * h ) <$> hs ) dupHist accumHistM
    in Map.intersectionWith (/) pondHist accumHist


otsuVariances :: E Color -> Bitmap -> HistogramF
otsuVariances method bitmap = Map.intersectionWith (+)
    (Map.intersectionWith (*)
        (fromIntegral <$> accumHistogram (scanl1 (+)) method bitmap)
        (accumHistVar (tail . inits) (scanl1 (+)) method bitmap))
    (Map.intersectionWith (*)
        (fromIntegral <$> accumHistogram (tail . scanr (+) 0) method bitmap)
        (accumHistVar (init . tails) (tail . scanr (+) 0) method bitmap))


segmentation :: E Color -> (Bool -> Color) -> E Bitmap
segmentation method picker bitmap =
    let minVar = snd $ minimum $ swap <$> Map.toList (otsuVariances method bitmap)
    in bitmap & pixels %~ picker . (> minVar) . view red . method


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


dynamicLevel :: Histogram -> Int
dynamicLevel = length


adaptContrast :: Word8 -> Word8 -> E Color -> E Bitmap
adaptContrast pmin' pmax' method bitmap =
    let b = bitmap & pixels %~ fromEnum . view red . method
        b' = concat b
        (plow, phigh) = (minimum b', maximum b')
        (pmin, pmax) = (fromEnum pmin', fromEnum pmax')
    in  b & pixels %~ \ p -> grayScale (toEnum $ (p - plow) *
            (pmax - pmin) `div` (phigh - plow) + pmin) 1


linearEqualization :: E Color -> E Bitmap
linearEqualization method bitmap =
    let dim = uncurry (*) $ bitmapDimensions bitmap
        b = bitmap & pixels %~ view red . method
        h = accumHistogram (scanl1 (+)) method bitmap <&> (`div` dim) . (*0xFF) . fromInteger
    in  b & pixels %~ \ p -> grayScale (toEnum $ h ^. at p . non 0) 1

