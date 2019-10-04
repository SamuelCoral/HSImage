{-# LANGUAGE DeriveFunctor #-}
module ImageProcessing.Filters where

import Control.Comonad
import Data.List
import Control.Lens
import ImageProcessing.Types
import ImageProcessing.Color
import GHC.Word
import Data.Foldable


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


addBorder :: Num a => Int -> Int -> E [[a]]
addBorder x y b =
    let hGap = replicate y $ replicate (length (head b) + 2 * x) (-1)
        vGap = replicate x (-1)
    in hGap ++ ((++ vGap) . (vGap ++) <$> b) ++ hGap


floatToWord8 :: Float -> Word8
floatToWord8
    v   | v < 0     = 0
        | v > 0xFF  = 0xFF
        | otherwise = round v


conv :: E [Float] -> (Float -> Word8) -> [[Float]] -> E Bitmap
conv mop pop m b =
    let b' = b & pixels %~ fromIntegral . view red . colorAverage
        ((w, h), (x, y)) = _2 . both %~ (`div` 2) $
            (b', m) & both %~ \ p -> (length $ head p, length p)
    in take h $ map (take w) $ getGrid $ Grid (addBorder x y b') =>> \ (Grid n) ->
        let v = pop $ sum $ uncurry (zipWith (*)) $ fmap mop $ unzip $
                filter ((>= 0) . fst) $ concat $ zipWith zip n m
        in RGBA v v v 1


(<**>) :: [[Float]] -> E Bitmap
m <**> b = conv normalizeVector round m b


(<|**|>) :: [[Float]] -> E Bitmap
m <|**|> b = conv id floatToWord8 m b


nonLinearFilter :: Int -> ([Word8] -> Word8) -> E Bitmap
nonLinearFilter n s b =
    let b' = b & pixels %~ fromEnum . view red . colorAverage
        (w, h) = (length $ head b, length b)
        x = n `div` 2
    in take h $ map (take w) $ getGrid $ Grid (addBorder x x b') =>> \ (Grid m) ->
        let v = s [ toEnum y | y <- concat $ take n <$> take n m, y >= 0 ]
        in RGBA v v v 1


median :: (Foldable t, Ord a) => t a -> a
median l = sort (toList l) !! (length l `div` 2)


gridBox :: Int -> [[Float]]
gridBox n = replicate n $ replicate n 1


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

