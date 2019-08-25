{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module ImageProcessing.Types where

import Control.Lens


data Color = RGBA {
    _red    :: Double,
    _green  :: Double,
    _blue   :: Double,
    _alpha  :: Double
} deriving (Show, Read, Eq, Ord)

$(makeLenses ''Color)


type Bitmap = [[Color]]

type E a = a -> a



transparent :: Color
transparent = RGBA 0 0 0 0


instance Semigroup Color where
    (RGBA r1 g1 b1 a1) <> (RGBA r2 g2 b2 a2) =
        let a3 = a1 + a2 * (1 - a1)
            [r3, g3, b3] = (\ (c1, c2) ->
                    (c1 * a1 + c2 * a2 * (1 - a1)) / a3
                ) <$> [(r1, r2), (g1, g2), (b1, b2)]
        in RGBA r3 g3 b3 a3


instance Monoid Color where
    mempty = transparent


pixels :: Traversal' Bitmap Color
pixels = traverse . traverse


pixel :: (Int, Int) -> Traversal' Bitmap Color
pixel (x, y) = ix y . ix x


getPixel :: (Int, Int) -> Bitmap -> Color
getPixel (x, y) bitmap = case bitmap ^? pixel (x, y) of
    Just p  -> p
    _       -> transparent
    

putPixel :: (Int, Int) -> Color -> E Bitmap
putPixel (x, y) color bitmap = bitmap & pixel (x, y) .~
    (color <> getPixel (x, y) bitmap)


groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n l = 
    let (h, t) = splitAt n l
    in [h] ++ groupsOf n t


bitmapDimensions :: Bitmap -> (Int, Int)
bitmapDimensions (r:rs) = (length r, length (r:rs))
bitmapDimensions _ = (0, 0)


sampleBitmap :: Bitmap
sampleBitmap = [ [
    RGBA (x / 800) (y / 600) (sqrt ((x - 400)^2 + (y - 300)^2) / 500) 1
    | x <- [0..800] ]
    | y <- [0..600] ]

