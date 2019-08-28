module ImageProcessing.Color where

import ImageProcessing.Types
import Data.Bits
import GHC.Word


colorMask :: Color -> E Color
colorMask (RGBA fr fg fb fa) (RGBA r g b a) =
    RGBA (fr .&. r) (fg .&. g) (fb .&. b) (fa .&. a)


grayScale :: Word8 -> Word8 -> Color
grayScale s a = RGBA s s s a


luma :: Float -> Float -> Float -> E Color
luma fr fg fb (RGBA r g b a) = grayScale
    (round $ sum $ zipWith (*) [fr, fg, fb] $ fromIntegral <$> [r, g, b]) a


colorAverage :: E Color
colorAverage (RGBA r g b a) = grayScale (toEnum $ (sum $ fromEnum <$> [r, g, b]) `div` 3) a


lumaPhotoshop :: E Color
lumaPhotoshop = luma 0.3 0.59 0.11


lumaBT709 :: E Color
lumaBT709 = luma 0.2126 0.7152 0.0722


lumaBT601 :: E Color
lumaBT601 = luma 0.299 0.587 0.114


desaturation :: E Color
desaturation (RGBA r g b a) = grayScale
    (toEnum $ (sum $ fromEnum <$> [maximum [r, g, b], minimum [r, g, b]]) `div` 2) a


maxDecomposition :: E Color
maxDecomposition (RGBA r g b a) = grayScale (maximum [r, g, b]) a


minDecomposition :: E Color
minDecomposition (RGBA r g b a) = grayScale (minimum [r, g, b]) a


repeatRedChannel :: E Color
repeatRedChannel (RGBA r _ _ a) = grayScale r a


repeatGreenChannel :: E Color
repeatGreenChannel (RGBA _ g _ a) = grayScale g a


repeatBlueChannel :: E Color
repeatBlueChannel (RGBA _ _ b a) = grayScale b a


grayShades :: Int -> E Color
grayShades n (RGBA r g b a) =
    let m = 0xFF / fromIntegral (pred n)
    in grayScale (round $ (fromIntegral $ round $
        (sum $ fromIntegral <$> [r, g, b]) / (3 * m)) * m) a


onlyRedChannel :: E Color
onlyRedChannel (RGBA r _ _ a) = RGBA r 0 0 a


onlyGreenChannel :: E Color
onlyGreenChannel (RGBA _ g _ a) = RGBA 0 g 0 a


onlyBlueChannel :: E Color
onlyBlueChannel (RGBA _ _ b a) = RGBA 0 0 b a


invertColor :: E Color
invertColor (RGBA r g b a) = RGBA (complement r) (complement g) (complement b) a

