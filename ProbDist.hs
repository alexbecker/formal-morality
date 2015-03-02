-- Defines the ProbDist monad and implements associated functions
module ProbDist where

import Data.Tuple
import Data.List
import Data.Function
import Control.Monad
import Control.Applicative
import Norm

data ProbDist a = ProbDist [(a, Double)]
	deriving (Eq, Show)

instance (Ord a) => Ord (ProbDist a) where
	compare (ProbDist xs) (ProbDist ys) = compare (sortBy (compare `on` fst) xs) (sortBy (compare `on` snd) ys)

instance Functor ProbDist where
	fmap f = ProbDist . map (\(x, y) -> (f x, y)) . getPairs

instance Monad ProbDist where
	x >>= f = flatten $ ProbDist $ zip (map f $ getEvents x) $ getProbs x
	return x = ProbDist [(x, 1.0)]

instance Applicative ProbDist where
	pure = return
	(<*>) = ap

instance (Ord a) => Norm (ProbDist a) where
	add x y = ProbDist $ zip (getEvents $ canonicalOrder x) $ map (uncurry (+)) $ zip (getProbs $ canonicalOrder x) (getProbs $ canonicalOrder y)
	scale d = ProbDist . map (\(y, z) -> (y, d * z)) . getPairs
	norm = norm . getProbs

getPairs :: ProbDist a -> [(a, Double)]
getPairs (ProbDist x) = x

getEvents :: ProbDist a -> [a]
getEvents (ProbDist x) = map fst x

getProbs :: ProbDist a -> [Double]
getProbs (ProbDist x) = map snd x

canonicalOrder :: (Ord a) => ProbDist a -> ProbDist a
canonicalOrder (ProbDist xs) = ProbDist (sortBy (compare `on` fst) xs)

-- this is just the monadic join
flatten :: ProbDist (ProbDist a) -> ProbDist a
flatten p = ProbDist $ concat $ map (\(x, y) -> zip (getEvents x) $ map (y *) $ getProbs x) $ getPairs p

-- combine equal events
reduce :: (Eq a) => ProbDist a -> ProbDist a
reduce dist = ProbDist $ map (\x -> (fst $ head x, sum $ map snd x)) $ groupBy ((==) `on` fst) $ getPairs dist
