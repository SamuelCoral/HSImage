module ImageProcessing.Types where


data Color = RGBA Double Double Double Double

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

