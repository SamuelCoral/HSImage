{-# LANGUAGE DeriveFunctor #-}
module ImageProcessing.Filters where

import Control.Comonad
import Data.List
import Control.Lens
import ImageProcessing.Types
import ImageProcessing.Color
import GHC.Word
import Data.Foldable
import Data.Maybe
import Data.Tuple


newtype Grid a = Grid { getGrid :: [[a]] } deriving (Functor, Eq, Ord)

instance Show a => Show (Grid a) where
    show (Grid m) = m >>= (++ "\n") . show


instance Comonad [] where
    extract = head
    duplicate = init . tails


duplicateHorizontal :: [[a]] -> [[[a]]]
duplicateHorizontal ([x]:xs) = [[x]:xs]
duplicateHorizontal m = m : duplicateHorizontal (tail <$> m)


duplicateMatrix :: [[a]] -> [[[[a]]]]
duplicateMatrix = map duplicateHorizontal . duplicate


instance Comonad Grid where
    extract = head . head . getGrid
    duplicate = Grid . map (map Grid) . duplicateMatrix . getGrid


normalizeVector :: E [Float]
normalizeVector m =
    let s = sum m
    in (/s) <$> m


addBorder :: Int -> Int -> [[a]] -> [[Maybe a]]
addBorder x y b =
    let hGap = replicate y $ replicate (length (head b) + 2 * x) Nothing
        vGap = replicate x Nothing
    in hGap ++ ((++ vGap) . (vGap ++) . map Just <$> b) ++ hGap


floatToWord8 :: Float -> Word8
floatToWord8
    v   | v < 0     = 0
        | v > 0xFF  = 0xFF
        | otherwise = round v


boolToWord8 :: Bool -> Word8
boolToWord8 b = if b then 0xFF else 0


filterImage :: E [a] -> (a -> Word8) -> (Word8 -> a) -> (a -> E a) -> ([a] -> a) -> [[a]] -> E Bitmap
filterImage mop pop ptr inner outter m b =
    let b' = b & pixels %~ ptr . view red . colorAverage
        ((w, h), (x, y)) = _2 . both %~ (`div` 2) $
            (b', m) & both %~ \ p -> (length $ head p, length p)
    in take h $ map (take w) $ getGrid $ Grid (addBorder x y b') =>> \ (Grid n) ->
        let v = pop $ outter $ uncurry (zipWith inner) $ fmap mop $
                swap $ unzip $ mapMaybe sequence $ concat $ zipWith zip m n
        in RGBA v v v 1


(<**>) :: [[Float]] -> E Bitmap
(<**>) = filterImage normalizeVector round fromIntegral (*) sum


(<|**|>) :: [[Float]] -> E Bitmap
(<|**|>) = filterImage id floatToWord8 fromIntegral (*) sum


nonLinearFilter :: Int -> ([Word8] -> Word8) -> E Bitmap
nonLinearFilter n s = filterImage id id id const s $ squareMatrix n 0


(<*|*>) :: [[Bool]] -> E Bitmap
(<*|*>) = filterImage id boolToWord8 (>= 0x80) (&&) or


(<*&*>) :: [[Bool]] -> E Bitmap
(<*&*>) = filterImage id boolToWord8 (>= 0x80) (\ p q -> not q || p) and


(<*|&*>) :: [[Bool]] -> E Bitmap
m <*|&*> b = m <*|*> (m <*&*> b)


(<*&|*>) :: [[Bool]] -> E Bitmap
m <*&|*> b = m <*&*> (m <*|*> b)


edgeDetection :: [[Bool]] -> E Bitmap
edgeDetection m b = zipWith (zipWith (\ p q -> grayScale (boolToWord8 $ p /= q) 1))
    (m <*&*> b) (m <*|*> b)


median :: (Foldable t, Ord a) => t a -> a
median l = sort (toList l) !! (length l `div` 2)


squareMatrix :: Int -> a -> [[a]]
squareMatrix n = replicate n . replicate n


gridBox :: Int -> [[Float]]
gridBox n = squareMatrix n 1


gaussian :: Int -> [[Float]]
gaussian n =
    let n' = fromIntegral n
        s = n' / 12
        v = s * s
        f = 1 / (2 * pi * v)
        c = n' / 2
    in [ [ f * exp (-((x - c)^2 + (y - c)^2) / (2 * v)) |
            x <- [0.5..pred n'] ] |
            y <- [0.5..pred n'] ]

