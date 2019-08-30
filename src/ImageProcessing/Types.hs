{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module ImageProcessing.Types where

import Control.Lens
import GHC.Word


data Color = RGBA {
    _red    :: Word8,
    _green  :: Word8,
    _blue   :: Word8,
    _alpha  :: Float
} deriving (Show, Read, Eq, Ord)

$(makeLenses ''Color)


type Bitmap = [[Color]]

type Point = (Int, Int)

type E a = a -> a



transparent :: Color
transparent = RGBA 0 0 0 0


instance Semigroup Color where
    (RGBA r1 g1 b1 a1) <> (RGBA r2 g2 b2 a2)
        | a1 == 0 = RGBA r2 g2 b2 a2
        | a1 == 1 = RGBA r1 g1 b1 a1
        | otherwise =
            let a3 = a1 + a2 * (1 - a1)
                [r3, g3, b3] = round . (\ (c1, c2) ->
                        (c1 * a1 + c2 * a2 * (1 - a1)) / a3
                    ) . (\ (p, q) -> (fromIntegral p, fromIntegral q)) <$>
                    [(r1, r2), (g1, g2), (b1, b2)]
            in RGBA r3 g3 b3 a3


instance Monoid Color where
    mempty = transparent


pixels :: Traversal Bitmap [[b]] Color b
pixels = traverse . traverse


pixel :: Point -> Traversal' Bitmap Color
pixel (x, y) = ix y . ix x


getPixel :: Point -> Bitmap -> Color
getPixel p bitmap = case bitmap ^? pixel p of
    Just c  -> c
    _       -> transparent
    

putPixel :: Point -> Color -> E Bitmap
putPixel p color bitmap = bitmap & pixel p .~
    color <> getPixel p bitmap


groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n l = 
    let (h, t) = splitAt n l
    in [h] ++ groupsOf n t


bitmapDimensions :: Bitmap -> Point
bitmapDimensions (r:rs) = (length r, length (r:rs))
bitmapDimensions _ = (0, 0)


sampleBitmap :: Bitmap
sampleBitmap = [ [
    RGBA (toEnum $ 0xFF * x `div` 800)
        (toEnum $ 0xFF * y `div` 600)
        (round $ 0xFF * sqrt ((fromIntegral x - 400)^2 + (fromIntegral y - 300)^2) / 500)
        1
    | x <- [1..800] ]
    | y <- [1..600] ]

