{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core



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
