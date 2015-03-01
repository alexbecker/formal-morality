module Morality where

import Data.Maybe
import Data.List
import Control.Applicative
import Preorder
import ProbDist
import Norm

type WorldState = String

type PossibleState = ProbDist WorldState

type MoralTheory = PreorderFamily PossibleState

invert :: (Eq a) => (a -> a) -> [a] -> a -> a
invert f domain x = fromJust $ find ((== x) . f) domain

-- infer a moral theory from adjacency lists approximating each preorder
-- and functions from your own states to those of others
inferMoralTheory :: [AdjacencyList PossibleState] -> [WorldState -> WorldState] -> MoralTheory
inferMoralTheory adjLists fs = isoSubFamily $ map (\n -> (fromAdjListNormed $ adjLists !! n, map (\m -> fmap $ (fs !! m) . invert (fs !! n) domain) [0..maxind])) [0..maxind] where
	maxind = length adjLists - 1
	domain = concat $ map getEvents $ map fst $ head adjLists

-- randomize a list of possible states across the moral community
veil :: MoralTheory -> PossibleState -> PossibleState
veil theory state = scale (1.0 / fromIntegral (length theory)) $ foldr1 add $ (snd $ head theory) <*> return state

-- same as above, but ignore self (assumes self's preferences have index 0)
veilMinusSelf :: MoralTheory -> PossibleState -> PossibleState
veilMinusSelf theory state = scale (1.0 / fromIntegral (length theory - 1)) $ foldr1 add $ (tail $ snd $ head theory) <*> return state

-- judges which state is Just (punnyness >9000), or Nothing
judge :: MoralTheory -> PossibleState -> PossibleState -> Maybe PossibleState
judge theory state1 state2 = case (comp (veil theory state1) (veil theory state2), comp (veilMinusSelf theory state1) (veilMinusSelf theory state2)) of
	(PGT, PGT) -> Just state1
	(PLT, PLT) -> Just state2
	_ -> Nothing
	where
		comp = pCompare $ fst $ head theory
