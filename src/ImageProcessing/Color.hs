module ImageProcessing.Color where

import ImageProcessing.Types


colorMask :: Color -> E Color
colorMask (RGBA fr fg fb fa) (RGBA r g b a) = RGBA (fr * r) (fg * g) (fb * b) (fa * a)


grayScale :: Double -> Double -> Color
grayScale s a = RGBA s s s a


luma :: Color -> E Color
luma (RGBA fr fg fb _) (RGBA r g b a) = grayScale (fr * r + fg * g + fb * b) a


colorAverage :: E Color
colorAverage (RGBA r g b a) = grayScale ((r + g + b) / 3) a


lumaPhotoshop :: E Color
lumaPhotoshop = luma $ RGBA 0.3 0.59 0.11 1


lumaBT709 :: E Color
lumaBT709 = luma $ RGBA 0.2126 0.7152 0.0722 1


lumaBT601 :: E Color
lumaBT601 = luma $ RGBA 0.299 0.587 0.114 1


desaturation :: E Color
desaturation (RGBA r g b a) = grayScale ((maximum [r, g, b] + minimum [r, g, b]) / 2) a


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
    let f = 1 / fromIntegral (pred n)
    in grayScale ((((r + g + b) / 3 / f) + 0.5) * f) a


onlyRedChannel :: E Color
onlyRedChannel (RGBA r _ _ a) = RGBA r 0 0 a


onlyGreenChannel :: E Color
onlyGreenChannel (RGBA _ g _ a) = RGBA 0 g 0 a


onlyBlueChannel :: E Color
onlyBlueChannel (RGBA _ _ b a) = RGBA 0 0 b a

