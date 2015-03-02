module Main where

import Data.Tuple
import Morality
import Preorder
import ProbDist

worldStates :: [WorldState]
worldStates = ["Alice falling in love", "Bob falling in love", "Both getting $10,000"]

-- convenient way to write probability distributions
unpack :: [Double] -> PossibleState
unpack = ProbDist . zip worldStates

-- alice strictly prefers at any probability herself falling in love to bob falling in love, 
-- and bob falling in love to both getting $10,000
-- conveniently this lines up with the derived total order on the ProbDists
alicePref :: Preorder PossibleState
alicePref = fromTotalOrder

-- bob values his own love at probability x as much as money at probability 3x, but prefers both strictly to alice's love
bobPref :: Preorder PossibleState
bobPref = Preorder (\x -> (\y -> case compare (3.0 * probN x 1 + probN x 2) (3.0 * probN y 1 + probN y 2) of
				LT -> PLT
				GT -> PGT
				EQ -> case compare (probN x 0) (probN x 1) of
					LT -> PLT
					GT -> PGT
					EQ -> EQV
			))
	where
		probN x n = getProbs x !! n

-- Alice recognizes' Bob's value of his own love to be substantially similar to her own,
-- and simililarly his appreciation of his friend's love and of his and his friend's money
aliceBobCorrespondence :: WorldState -> WorldState
aliceBobCorrespondence "Alice falling in love" = "Bob falling in love"
aliceBobCorrespondence "Bob falling in love" = "Alice falling in love"
aliceBobCorrespondence "Both getting $10,000" = "Both getting $10,000"

-- same correspondence, lifted to PossibleStates
abcLifted :: PossibleState -> PossibleState
abcLifted = canonicalOrder . fmap aliceBobCorrespondence

-- inverse of above
bacLifted :: PossibleState -> PossibleState
bacLifted = canonicalOrder . fmap (invert aliceBobCorrespondence worldStates)

aliceTestTheory :: MoralTheory
aliceTestTheory = isoSubFamily [(alicePref, [id, abcLifted]), (bobPref, [bacLifted, id])]

bobTestTheory :: MoralTheory
bobTestTheory = isoSubFamily [(bobPref, [id, bacLifted]), (alicePref, [abcLifted, id])]

exampleAlicePairs :: [(String, PossibleState, PossibleState)]
exampleAlicePairs = [("Alice must choose Bob falling in love over the money, because both agree that the veiled situation of falling in love half the time is more valuable than 100% chance of getting the money. The unveiled situation happens to agree with Alice's personal values as well.",
						unpack [0.0, 1.0, 0.0], unpack [0.0, 0.0, 1.0]),
					("Alice values falling in love over getting the money, and Bob is indifferent for his equivalent situation. This would result in a duty to Alice, except it would be a duty to herself, which is impossible.", 
						unpack [1.0, 0.0, 0.0], unpack [0.0, 0.0, 1.0]),
					("Alice, were she in Bob's shoes, would value Bob falling in love over the money at any probability, but when her values are not imposed on Bob he does not value the options this way. Thus no duty is created.",
						unpack [0.75, 0.25, 0.0], unpack [0.0, 0.0, 1.0])]

exampleBobPairs :: [(String, PossibleState, PossibleState)]
exampleBobPairs = [("The second case above, from Bob's perspective. While Alice cannot have a duty to herself, Bob can have a duty to her. Note that the unveiled situation disagrees with Bob's own values, so in this case the moral theory has nontrivial implications.", 
						unpack [1.0, 0.0, 0.0], unpack [0.0, 0.0, 1.0])]

main :: IO ()
main = do
	putStrLn $ "Worldstates: " ++ show worldStates
	putStrLn "Alice's values: Lexicographical (Alice in love, Bob in love, money)"
	putStrLn "Bob's values: Bob in love with probability x is valued the same as money with probability 3x, Alice in love is valued least."
	putStrLn "Alice -> Bob Correspondence: "
	sequence_ $ map (\ws -> putStrLn $ '\t' : ws ++ " -> " ++ aliceBobCorrespondence ws) worldStates
	putStrLn "\nAlice's duties: "
	sequence_ $ map (\(d, a, b) -> putStrLn ('\t' : show (a, b)) >> putStrLn ("\tJudgement: " ++ show (judge aliceTestTheory a b)) >> putStrLn ('\t' : d ++ "\n")) exampleAlicePairs
	putStrLn "\nBob's duties: "
	sequence_ $ map (\(d, a, b) -> putStrLn ('\t' : show (a, b)) >> putStrLn ("\tJudgement: " ++ show (judge bobTestTheory a b)) >> putStrLn ('\t' : d ++ "\n")) exampleBobPairs
