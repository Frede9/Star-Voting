{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
import Data.List (sortBy)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map


data Results = Results {
    rlist :: [(Candidate, Double)]
} deriving (Show, Eq)

data Candidate = Candidate String deriving (Show, Eq, Ord)

data Vote = Vote {
    candidate :: Candidate,
    stars :: Double
} deriving (Show, Eq)


data Voter = Voter {
  votes :: [Vote]
} deriving (Show, Eq)

main = do
  voters <- runX (parseXML "example.xml" >>> getVotes)
  let myMap  = Map.empty
  let listOfVoters = getListOfVoters voters
  let resultList = Results {rlist = getResults listOfVoters myMap}
  let besteKandidaten = getRunoffCanidates resultList
  let filtered = filterRunoffCanidatesVotes listOfVoters besteKandidaten
  let runoff = doRunoff (rlist besteKandidaten) filtered
  print besteKandidaten
  putStrLn "Gefilterte Votes nach dem RunOff\n"
  print filtered
  putStrLn "Runoff\n"
  print runoff

reElection a = do
  return expression

getListOfVoters :: [Voter] -> [Voter]
getListOfVoters [] = []
getListOfVoters (x:xs)
    | checkVotes (votes x) candidateList == False = getListOfVoters xs
	  | otherwise = x : getListOfVoters xs

getResults :: [Voter] -> Map Candidate Double -> [(Candidate, Double)]
getResults [] mymap = Map.toList mymap
getResults (x:xs) mymap = getResults xs (insertVotes (votes x) (mymap))

insertVotes :: [Vote] -> Map Candidate Double -> Map Candidate Double
insertVotes [] mymap = mymap
insertVotes (x:xs) mymap = insertVotes xs (Map.insertWith (+) (candidate x) (stars x) mymap)

checkVotes :: [Vote] -> [Candidate] -> Bool
checkVotes [] [] = True
checkVotes x [] = False
checkVotes [] y = False
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
    returnA -< Vote {candidate = Candidate candidate, stars = read vote::Double}

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
        | snd(last(xs)) == snd(y) = checkIfSameScore (init xs) ((last xs):(y:ys))
        | otherwise = (y:ys)


filterRunoffCanidatesVotes :: [Voter] -> Results -> [Voter]
filterRunoffCanidatesVotes [] _ = []
filterRunoffCanidatesVotes (x:xs) y = sort([] ++ [Voter (helpFunctionFORCV (votes x) y)] ++ filterRunoffCanidatesVotes xs y)


helpFunctionFORCV :: [Vote] -> Results -> [Vote]
helpFunctionFORCV [] _ = []
helpFunctionFORCV (x:xs) y
      | firstCandidate == (candidate x) = x : helpFunctionFORCV xs y
      | secondCandidate == (candidate x) = x : helpFunctionFORCV xs y
      | otherwise  = helpFunctionFORCV xs y
        where
          firstCandidate = fst(head(rlist y))
          secondCandidate = fst(last(rlist y))

sort :: [Voter] -> [Voter]
sort [] = []
sort (x:xs) = [] ++ [Voter (sortBy (compare `on` candidate) (votes x))] ++ sort xs


-- Zum Testen doRunoff (rlist(getRunoffCanidates countedResults)) filterVotes
-- filterVotes wird aus filterRunoffCanidatesVotes gezogen
-- Ablauf ist jetzt x = getRunoffCanidates countedResults
-- y = filterRunoffCanidatesVotes correctVotes x
-- doRunoff (rlist x) y
doRunoff :: [(Candidate, Double)] -> [Voter] -> [(Candidate, Double)]
doRunoff [] _ = []
doRunoff (x:xs) ys =  [(canidateName,countWins canidateName ys )] ++ doRunoff xs ys
  where
    canidateName = fst(x)



countWins :: Candidate -> [Voter] -> Double
countWins _ [] = 0
countWins x (y:ys)
  | length(votes(y)) == 2 =                  --Wenn Länge zwei ist, sind beide Kanidaten vorhanden und es kann verglichen geguckt werden
    if canidateName == x                      --ob der gesuchte Kanidat Vorne oder hinten ist, dementsprechend wird ein Punkt draufgerechnet
    then if fstVoteResult > sndVoteResult
          then 1 + countWins x ys
          else countWins x ys
    else if fstVoteResult < sndVoteResult
          then 1 + countWins x ys
          else countWins x ys
  | length(votes(y)) == 1 =                  -- Ist die Länge eins, muss geschaut werden ob der Kanidat überhaupt in der Liste steht
    if canidateName == x                    -- falls ja, bekommt er einen Punkt, sollte er nicht drin stehen, nicht
      then 1 + countWins x ys
      else countWins x ys
  | otherwise = countWins x ys
      where
        canidateName = candidate(head(votes y))
        fstVoteResult = stars(head(votes(y)))
        sndVoteResult = stars(last(votes(y)))


candidateList = [kandidat7,kandidat8,kandidat9,kandidat10]
