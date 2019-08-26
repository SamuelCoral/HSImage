module ImageProcessing.BMP where

import ImageProcessing.Types
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary.Put
import GHC.Word
import Control.Monad



toBytes :: Int -> Int -> [Word8]
toBytes n = map (toEnum . (.&. 0xFF)) . take n . iterate (`shiftR` 8)


fromBytes :: [Word8] -> Int
fromBytes = foldr ((.|.) . (\ (i, x) -> fromEnum x `shiftL` i)) 0 . zip [0, 8..]


rowSize :: Int -> Int
rowSize width = ceiling (fromIntegral (width * 3) / 4) * 4


generateBMPHeader :: Int -> Int -> [Word8]
generateBMPHeader width height =
    let headerSize = 54
        bitmapSize = rowSize width * height
        fileSize = headerSize + bitmapSize
    in (toEnum . fromEnum <$> "BM") ++
        (toBytes 4 =<< [fileSize, 0, headerSize, 40, width, height]) ++
        (toBytes 2 =<< [1, 24]) ++
        (toBytes 4 =<< [0, bitmapSize, 2835, 2835, 0, 0])


extractBMPHeaderDimensions :: [Word8] -> (Int, Int)
extractBMPHeaderDimensions header = (
        fromBytes $ take 4 (drop 18 header),
        fromBytes $ take 4 (drop 22 header)
    )


bitmapFromBytes :: Int -> [Word8] -> Bitmap
bitmapFromBytes width s =
    reverse $ map (\ p ->
        let [b, g, r] = (/ 0xFF) . fromIntegral . fromEnum <$> p
        in RGBA r g b 1
    ) . take width . groupsOf 3 <$> groupsOf (rowSize width) s


bitmapToBytes :: Bitmap -> [Word8]
bitmapToBytes [] = []
bitmapToBytes bitmap =
    let width = length $ head bitmap
        gap = replicate (rowSize width - (width * 3)) 0
    in (++ gap) =<< reverse (concatMap (\ (RGBA r g b _) ->
            toEnum . (`mod` 0x100) . round . (* 0xFF) <$> [b, g, r]
        ) <$> bitmap)


readBMP :: FilePath -> IO Bitmap
readBMP file = do
    input <- BS.readFile file
    let str = runGet (replicateM (BS.length input) getWord8) $ BL.fromStrict input
        (width, _) = extractBMPHeaderDimensions str
    return $ bitmapFromBytes width (drop 54 str)


saveBMP :: FilePath -> Bitmap -> IO ()
saveBMP out bitmap =
    let (width, height) = bitmapDimensions bitmap
    in BL.writeFile out $ runPut $ mapM_ putWord8 $
        generateBMPHeader width height ++ bitmapToBytes bitmap

