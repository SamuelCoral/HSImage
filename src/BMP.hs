module BMP where

import Data.Bits



toString :: Int -> Int -> String
toString n = map (toEnum . (.&. 0xFF)) . take n . iterate (`shiftR` 8)


fromString :: String -> Int
fromString = foldr ((.|.) . (\ (i, x) -> fromEnum x `shiftL` i)) 0 . zip [0, 8..]


generateBMPHeader :: Int -> Int -> String
generateBMPHeader width height =
    let headerSize = 54
        bitmapSize = ceiling (fromIntegral (width * 3) / 4) * 4 * height
        fileSize = headerSize + bitmapSize
    in "BM" ++ (
            toString 4 =<< [fileSize, 0, headerSize, 40, width, height]
        ) ++ (
            toString 2 =<< [1, 24]
        ) ++ (
            toString 4 =<< [0, bitmapSize, 2835, 2835, 0, 0]
        )


extractBMPHeaderDimensions :: String -> (Int, Int)
extractBMPHeaderDimensions header = (
        fromString $ take 4 (drop 18 header),
        fromString $ take 4 (drop 22 header)
    )

