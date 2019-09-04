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


type Vector = (Float, Float)


pointToVector :: Point -> Vector
pointToVector (x, y) = (fromIntegral x, fromIntegral y)


vectorToPoint :: Vector -> Point
vectorToPoint (x, y) = (truncate x, truncate y)


instance Num Matrix where
    (Matrix a b c d) + (Matrix e f g h) = Matrix (a + e) (b + f) (c + g) (d + h)
    (Matrix a b c d) - (Matrix e f g h) = Matrix (a - e) (b - f) (c - g) (d - h)
    (Matrix a b c d) * (Matrix e f g h) = Matrix
        (a * e + b * g) (a * f + b * h)
        (c * e + d * g) (c * f + d * h)
    abs (Matrix a b c d) =
        let det = a * d - b * c
        in Matrix det 0 0 det
    signum (Matrix a b c d) =
        let f = 1 / (a * d - b * c)
        in Matrix (a * f) (b * f) (c * f) (d * f)
    fromInteger n = Matrix (fromInteger n) 0 0 (fromInteger n)


instance Fractional Matrix where
    fromRational r = Matrix (fromRational r) 0 0 (fromRational r)
    recip (Matrix a b c d) =
        let f = 1 / (a * d - b * c)
        in Matrix (d * f) (-b * f) (-c * f) (a * f)


idMatrix :: Matrix
idMatrix = Matrix 1 0 0 1


rotationMatrix :: Float -> Matrix
rotationMatrix angle =
    let ca = cos angle
        sa = sin angle
    in Matrix ca (-sa) sa ca


infixl 6 `vadd`
vadd :: Vector -> E Vector
(a, b) `vadd` (c, d) = (a + c, b + d)


infixl 6 `vsub`
vsub :: Vector -> E Vector
(a, b) `vsub` (c, d) = (a - c, b - d)


infixr 7 *|
(*|) :: Matrix -> E Vector
(Matrix a b c d) *| (x, y) = (a * x + b * y, c * x + d * y)


fitIn :: Int -> Int -> a -> E [a]
fitIn expectedWidth pos fill row =
    let rowSize = length row
        firstPart = if pos < 0
            then drop (abs pos) row
            else replicate pos fill ++ row
        realRowLength = pos + rowSize
    in  take expectedWidth firstPart ++ if realRowLength >= expectedWidth
        then [] else replicate (expectedWidth - realRowLength) fill


pasteOver :: Point -> Bitmap -> E Bitmap
pasteOver (x, y) bitmap overBitmap =
    let (w, h) = bitmapDimensions overBitmap
        bitmap' = fitIn h y (replicate w transparent) $
            fitIn w x transparent <$> bitmap
    in  zipWith (zipWith (<>)) bitmap' overBitmap


transformBitmap :: Matrix -> Point -> Bitmap -> Point -> E Bitmap
transformBitmap m op b op' b' = 
    let m' = recip m
        o = pointToVector op
        o' = pointToVector op'
    in  (\ (r, l) ->
            (\ (c, pix) -> getPixel (vectorToPoint $
                m' *| ((c, r) `vsub` o') `vadd` o) b <> pix
            ) <$> zip [0..] l
        ) <$> zip [0..] b'

