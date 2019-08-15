module ImageProcessing.BMP where

import ImageProcessing.Types
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Binary.Put



toString :: Int -> Int -> String
toString n = map (toEnum . (.&. 0xFF)) . take n . iterate (`shiftR` 8)


fromString :: String -> Int
fromString = foldr ((.|.) . (\ (i, x) -> fromEnum x `shiftL` i)) 0 . zip [0, 8..]


rowSize :: Int -> Int
rowSize width = ceiling (fromIntegral (width * 3) / 4) * 4


generateBMPHeader :: Int -> Int -> String
generateBMPHeader width height =
    let headerSize = 54
        bitmapSize = rowSize width * height
        fileSize = headerSize + bitmapSize
    in "BM" ++
        (toString 4 =<< [fileSize, 0, headerSize, 40, width, height]) ++
        (toString 2 =<< [1, 24]) ++
        (toString 4 =<< [0, bitmapSize, 2835, 2835, 0, 0])


extractBMPHeaderDimensions :: String -> (Int, Int)
extractBMPHeaderDimensions header = (
        fromString $ take 4 (drop 18 header),
        fromString $ take 4 (drop 22 header)
    )


bitmapFromString :: Int -> String -> Bitmap
bitmapFromString width s =
    map (\ p ->
        let [b, g, r] = (/ 0xFF) . fromIntegral . fromEnum <$> p
        in (r, g, b)
    ) . take width . groupsOf 3 <$> groupsOf (rowSize width) s


bitmapToString :: Bitmap -> String
bitmapToString [] = ""
bitmapToString bitmap =
    let width = length $ head bitmap
        gap = replicate (rowSize width - (width * 3)) '\0'
    in (++ gap) =<< reverse (concatMap (\ (r, g, b) ->
            toEnum . (`mod` 0x100) . round . (* 0xFF) <$> [b, g, r]
        ) <$> bitmap)


saveBMP :: FilePath -> Bitmap -> IO ()
saveBMP out bitmap =
    let (width, height) = bitmapDimensions bitmap
    in BS.writeFile out $ runPut $ mapM_ putWord8 $ toEnum . fromEnum <$>
        generateBMPHeader width height ++ bitmapToString bitmap

