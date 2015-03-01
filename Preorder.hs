-- Defines the Preorder type and implements associated functions
module Preorder where

import Data.List
import Data.Tuple
import Data.Maybe
import Data.Function
import Norm

data Preordering = PLT	-- less than
				 | PGT	-- greater than
				 | EQV	-- equivalent
				 | INC	-- incomparable
	deriving (Eq)

instance Show Preordering where
	show PLT = "<="
	show PGT = ">="
	show EQV = "~="
	show INC = "!!"

instance Read Preordering where
	readsPrec _ s = case s of
		"<=" -> [(PLT, "")]
		">=" -> [(PGT, "")]
		"~=" -> [(EQV, "")]
		"!!" -> [(INC, "")]
		_ -> []

data Preorder a = Preorder (a -> a -> Preordering)

-- (a,bs) is such that b in bs if a >= b
type AdjacencyList a = [(a,[a])]

-- a set of partial orders with a complete set of functions between them
-- [(domain, function : domain -> range)]
-- domains and ranges must be in the same order
type PreorderFamily a = [(Preorder a, [a -> a])]

---- Preorder functions ----

pCompare :: Preorder a -> a -> a -> Preordering
pCompare (Preorder f) = f

fromTotalOrder :: (Ord a) => Preorder a
fromTotalOrder = Preorder (\x -> (\y ->
					case compare x y of
						LT -> PLT
						GT -> PGT
						EQ -> EQV
					))

inAdjList :: (Eq a) => AdjacencyList a -> a -> Bool
inAdjList adjList = flip elem $ map fst adjList

getAdj :: (Eq a) => AdjacencyList a -> a -> [a]
getAdj adjList a = snd $ fromJust $ find ((== a) . fst) adjList

fromAdjList :: (Eq a) => AdjacencyList a -> Preorder a
fromAdjList adjList = Preorder (\x -> (\y ->
						if inAdjList adjList x && inAdjList adjList y
							then if elem y (getAdj adjList x)
								then if elem x (getAdj adjList y)
									then EQV
									else PGT
								else if elem x (getAdj adjList y)
									then PLT
									else INC
							else INC
						))

toAdjList :: (Enum a) => Preorder a -> a -> AdjacencyList a
toAdjList (Preorder f) minElem = map (\x -> (x, filter (\y -> f x y == PGT || f x y == EQV) all)) all where
	all = enumFrom minElem

-- allows a preorder of infinitely many normed items to be defined using only finitely many entries
-- comparison uses the closest points defined
fromAdjListNormed :: (Norm a, Eq a) => AdjacencyList a -> Preorder a
fromAdjListNormed adjList = Preorder (pCompare p `on` nearest (map fst adjList)) where
	p = fromAdjList adjList

---- PreorderFamily functions ----

-- makes a minimal set of elements in each ordering incomparable
-- such that each map is an isomorphism on the set of elements comparable to at least one other
-- family must be finite
isoSubFamily :: PreorderFamily a -> PreorderFamily a
isoSubFamily family = foldr (.) id (map isoSubFamilyHelper [0 .. length family - 1]) family

-- makes incomparable any elements of the poset at index which disagree under one or more functions
isoSubFamilyHelper :: Int -> PreorderFamily a -> PreorderFamily a
isoSubFamilyHelper index family = map (\m -> (Preorder $ newPO m, snd $ family !! m)) [0 .. length family - 1] where
	newPO m x y = if all (== pCompare po x y) (map (\n -> pCompare (fpo n) (f n x) (f n y)) [0 .. length family - 1])
					then pCompare po x y
					else INC
		where
			po = fst $ family !! m
			fpo n = fst $ family !! n
			f n = (snd $ family !! m) !! n
