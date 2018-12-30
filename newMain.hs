{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
import Data.List (sortBy)
import Data.Function (on)

		 
data Results = Results {
    rlist :: [(Candidate, Double)]
} deriving (Show, Eq)

data Candidate = Candidate {
    name :: String
} deriving (Show, Eq)
  
data Vote = Vote {
    candidate :: Candidate,
    stars :: Double
} deriving (Show, Eq)

data Voter = Voter {
  votes :: [Vote]
} deriving (Show, Eq)



main = do
  voters <- runX (parseXML "example.xml" >>> getVotes)
  print voters
  
check :: [Voter] -> [Candidate] -> [Voter]
check (x:xs) cans
        | null (x:xs) = []
		| checkVotes (votes x) cans == False = check xs cans
		| otherwise = x : check xs cans
		 
checkVotes :: [Vote] -> [Candidate] -> Bool
checkVotes _ _ = True
checkVotes x _ = False
checkVotes _ y = False
checkVotes (x:xs) candidates
		 | stars x > 5 || stars x < 0 = False
		 | (elem) (candidate x) (candidates) = checkVotes (xs) (filter  (not.(==(candidate x))) candidates)
		 | otherwise = False


parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file
 
atTag tag = deep (isElem >>> hasName tag)

getVote = atTag "VOTE" >>>
  proc v -> do
    candidate    <- getAttrValue "CANDIDATE" -< v
    vote    <- getAttrValue  "RATING"    -< v
    returnA -< Vote {candidate = Candidate {name = candidate}, stars = read vote::Double}

getVotes = atTag "VOTER" >>>
  proc vs -> do
    votes <- listA getVote    -< vs
    returnA -< Voter {votes = votes}
    
    
    
    
    
getRunoffCanidates :: Results -> Results
getRunoffCanidates xs
        | null(rlist xs) = xs
        | length(rlist xs) <= 2 = xs
        | otherwise = Results (checkIfSameScore otherCanidates runoffCanidates)
          where
            sorted = sortBy (compare `on` snd) (rlist xs)
            otherCanidates = take (length sorted - 2) sorted
            runoffCanidates = drop (length sorted - 2) sorted


checkIfSameScore :: [(Candidate, Double)] -> [(Candidate, Double)] -> [(Candidate, Double)]
checkIfSameScore [] ys = ys
checkIfSameScore xs [] = []
checkIfSameScore xs (y:ys)
      | null (xs) = (y:ys)
      | newCanidateResult == runoffCanidateResult = checkIfSameScore (init xs) ((last xs):(y:ys))
      | otherwise = (y:ys)
        where
          newCanidateResult = snd(last(xs))
          runoffCanidateResult = snd(y)
	  
	  
-- Zum testen getRunoffCaindates mit der Resultlist aufrufen

kandidat1 = Candidate "kandidat1"
kandidat2 = Candidate "kandidat2"
kandidat3 = Candidate "kandidat3"
kandidat4 = Candidate "kandidat4"
kandidat5 = Candidate "kandidat5"
kandidat6 = Candidate "kandidat6"


resultList = Results [(kandidat1,5),(kandidat2,2),(kandidat3,3),(kandidat4,4),(kandidat5,3),(kandidat6,4)]
