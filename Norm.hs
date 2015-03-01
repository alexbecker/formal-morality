-- defines the Norm class and implements related functions
module Norm where

import Data.List
import Data.Function
import Control.Applicative

class Norm a where
	scale :: Double -> a -> a
	add :: a -> a -> a
	norm :: a -> Double

instance Norm Double where
	scale = (*)
	add = (+)
	norm = abs

instance (Norm a) => Norm [a] where
	scale = map . scale
	add x = map (uncurry add) . zip x
	norm = sum . map ((** 2) . norm)

sub :: (Norm a) => a -> a -> a
sub x = add x . scale (-1)

dist :: (Norm a) => a -> a -> Double
dist x = norm . sub x

nearest :: (Norm a) => [a] -> a -> a
nearest xs x = minimumBy (compare `on` (dist x)) xs
