module ImageProcessing.Types where


type Color = (Double, Double, Double, Double)

type Bitmap = [[Color]]

type E a = a -> a



transparent :: Color
transparent = (0, 0, 0, 0)


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
    (x / 800, y / 600, sqrt ((x - 400)^2 + (y - 300)^2) / 500, 1)
    | x <- [0..800] ]
    | y <- [0..600] ]

