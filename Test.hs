module Test where

import Morality
import Preorder
import ProbDist

-- convenient way of representing a probability distribution of world states
samplePoint :: [WorldState] -> [Double] -> ProbDist WorldState
samplePoint worldStates = ProbDist . zip worldStates

samplePoints :: [WorldState] -> [[Double]] -> [ProbDist WorldState]
samplePoints = map . samplePoint

alexWS :: [WorldState]
alexWS = ["Grey goo", "1984", "Status quo ante"]

-- alex's preference ordering is approximately the reverse lexicographical ordering with comparison order ("1984", "Grey goo", "Status quo ante")
-- format is an adjacency list where an edge (x,y) exists iff x >= y
alexPrefs :: AdjacencyList (ProbDist WorldState)
alexPrefs = [(samplePoint alexWS [1.0, 0.0, 0.0], samplePoints alexWS [[1.0, 0.0, 0.0], [0.666, 0.333, 0.0], [0.333, 0.666, 0.0], [0.333, 0.333, 0.333], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333], [0.0, 0.333, 0.666]]),
	(samplePoint alexWS [0.666, 0.333, 0.0], samplePoints alexWS [[0.666, 0.333, 0.0], [0.333, 0.666, 0.0], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333]]),
	(samplePoint alexWS [0.666, 0.0, 0.333], samplePoints alexWS [[1.0, 0.0, 0.0], [0.666, 0.333, 0.0], [0.666, 0.0, 0.333], [0.333, 0.666, 0.0], [0.333, 0.333, 0.333], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333], [0.0, 0.333, 0.666]]),
	(samplePoint alexWS [0.333, 0.666, 0.0], samplePoints alexWS [[0.333, 0.666, 0], [0.0, 1.0, 0.0]]),
	(samplePoint alexWS [0.333, 0.333, 0.333], samplePoints alexWS [[0.666, 0.333, 0], [0.333, 0.666, 0.0], [0.333, 0.333, 0.333], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333]]),
	(samplePoint alexWS [0.333, 0.0, 0.666], samplePoints alexWS [[1.0, 0.0, 0.0], [0.666, 0.333, 0.0], [0.666, 0.0, 0.333], [0.333, 0.666, 0.0], [0.333, 0.333, 0.333], [0.333, 0.0, 0.666], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333], [0.0, 0.333, 0.666]]),
	(samplePoint alexWS [0.0, 1.0, 0.0], samplePoints alexWS [[0.0, 1.0, 0.0]]),
	(samplePoint alexWS [0.0, 0.666, 0.333], samplePoints alexWS [[0.333, 0.666, 0], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333]]),
	(samplePoint alexWS [0.0, 0.333, 0.666], samplePoints alexWS [[0.666, 0.333, 0], [0.333, 0.666, 0], [0.333, 0.333, 0.333], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333], [0.0, 0.333, 0.666]]),
	(samplePoint alexWS [0.0, 0.0, 1.0], samplePoints alexWS [[1.0, 0.0, 0.0], [0.666, 0.333, 0.0], [0.666, 0.0, 0.333], [0.333, 0.666, 0.0], [0.333, 0.333, 0.333], [0.333, 0.0, 0.666], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333], [0.0, 0.333, 0.666], [0.0, 0.0, 1.0]])]

hallWS :: [WorldState]
hallWS = ["World War III", "Brave New World", "Status quo ante"]

-- alex recognizes hall's concept of World War III as substantially similar to his concept of the grey goo scenario, and "Brave New World" as "1984"
alexHallCorrespondence :: WorldState -> WorldState
alexHallCorrespondence "Grey goo" = "World War III"
alexHallCorrespondence "Brave New World" = "1984"
alexHallCorrespondence "Status quo ante" = "Status quo ante"

-- hall cannot compare between "World War III" and "Brave New World", but strictly prefers the status quo ante to either
hallPrefs :: AdjacencyList (ProbDist WorldState)
hallPrefs = [(samplePoint hallWS [1.0, 0.0, 0.0], samplePoints hallWS [[1.0, 0.0, 0.0]]),
	(samplePoint hallWS [0.666, 0.333, 0.0], samplePoints hallWS [[0.666, 0.333, 0.0]]),
	(samplePoint hallWS [0.666, 0.0, 0.333], samplePoints hallWS [[1.0, 0.0, 0.0], [0.666, 0.333, 0.0], [0.666, 0.0, 0.333], [0.333, 0.666, 0.0], [0.0, 1.0, 0.0]]),
	(samplePoint hallWS [0.333, 0.666, 0.0], samplePoints hallWS [[0.333, 0.666, 0]]),
	(samplePoint hallWS [0.333, 0.333, 0.333], samplePoints hallWS [[1.0, 0.0. 0.0], [0.666, 0.333, 0], [0.333, 0.666, 0.0], [0.333, 0.333, 0.333], [0.0, 1.0, 0.0]]),
	(samplePoint hallWS [0.333, 0.0, 0.666], samplePoints hallWS [[1.0, 0.0, 0.0], [0.666, 0.333, 0.0], [0.666, 0.0, 0.333], [0.333, 0.666, 0.0], [0.333, 0.333, 0.333], [0.333, 0.0, 0.666], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333]]),
	(samplePoint hallWS [0.0, 1.0, 0.0], samplePoints hallWS [[0.0, 1.0, 0.0]]),
	(samplePoint hallWS [0.0, 0.666, 0.333], samplePoints hallWS [[1.0, 0.0, 0.0], [0.666, 0.333, 0.0], [0.333, 0.666, 0], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333]]),
	(samplePoint hallWS [0.0, 0.333, 0.666], samplePoints hallWS [[1.0, 0.0, 0.0], [0.666, 0.333, 0], [0.666, 0.0, 0.333], [0.333, 0.666, 0], [0.333, 0.333, 0.333], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333], [0.0, 0.333, 0.666]]),
	(samplePoint hallWS [0.0, 0.0, 1.0], samplePoints hallWS [[1.0, 0.0, 0.0], [0.666, 0.333, 0.0], [0.666, 0.0, 0.333], [0.333, 0.666, 0.0], [0.333, 0.333, 0.333], [0.333, 0.0, 0.666], [0.0, 1.0, 0.0], [0.0, 0.666, 0.333], [0.0, 0.333, 0.666], [0.0, 0.0, 1.0]])]

alexTestTheory :: MoralTheory
alexTestTheory = inferMoralTheory [alexPrefs, hallPrefs] [alexHallCorrespondence]

hallTestTheory :: MoralTheory
hallTestTheory = inferMoralTheory [hallPrefs, alexPrefs] [invert alexHallCorrespondence alexWS]
