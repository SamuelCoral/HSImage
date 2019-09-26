{-# LANGUAGE DeriveFunctor #-}
module ImageProcessing.Filters where

import Control.Comonad
import Data.List
import Control.Lens
import ImageProcessing.Types
import ImageProcessing.Color


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
duplicateMatrix = fmap duplicateHorizontal . duplicate


instance Comonad Grid where
    extract = head . head . getGrid
    duplicate = Grid . map (map Grid) . duplicateMatrix . getGrid


normalizeVector :: E [Float]
normalizeVector m =
    let s = sum m
    in (/s) <$> m


conv :: [[Float]] -> E Bitmap
m `conv` b =
    let b' = b & pixels %~ fromIntegral . view red . colorAverage
        ((w, h), (x, y)) = _2 . both %~ (`div` 2) $
            (b', m) & both %~ \ p -> (length $ head p, length p)
        hGap = replicate y $ replicate (length (head b) + 2 * x) (-1)
        vGap = replicate x (-1)
        bb = hGap ++ ((++ vGap) . (vGap ++) <$> b') ++ hGap
    in take h $ map (take w) $ getGrid $ Grid bb =>> \ (Grid n) ->
        let v = round $ sum $ uncurry (zipWith (*)) $ fmap normalizeVector $ unzip $
                filter ((>= 0) . fst) $ concat $ zipWith zip n m
        in RGBA v v v 1


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

