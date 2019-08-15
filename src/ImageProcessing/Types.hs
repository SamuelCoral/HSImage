module ImageProcessing.Types where


type Pixel = (Double, Double, Double)

type Bitmap = [[Pixel]]


groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n l = 
    let (h, t) = splitAt n l
    in [h] ++ groupsOf n t


bitmapDimensions :: Bitmap -> (Int, Int)
bitmapDimensions (r:rs) = (length r, length (r:rs))
bitmapDimensions _ = (0, 0)

