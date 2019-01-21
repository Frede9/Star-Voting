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

election votesxml candidatesxml = do
  voters <- runX (parseXML votesxml >>> getVotes)
  --print voters
  candidates <- runX (parseXML candidatesxml >>> getCandidates)
  --print candidates
  let myMap  = Map.empty
      listOfVoters = getListOfVoters voters candidates
      resultList = Results {rlist = getResults listOfVoters myMap}
  if (length candidates > 2)
    then do
         let besteKandidaten = getRunoffCandidates resultList
         if (length (rlist besteKandidaten) > 2)
           then do
                newPath
           else do
                let filtered = sort (filterRunoffCandidatesVotes listOfVoters besteKandidaten)
                    runoff = doRunoff (rlist besteKandidaten) filtered
                print runoff --printing besteKandidaten filtered runoff
    else do
         let filtered = sort (filterRunoffCandidatesVotes listOfVoters resultList)
             runoff = doRunoff (rlist resultList) filtered
         print runoff --printing resultList filtered runoff

--Ausgabe von allen Daten
printing best filtered runoff = do
                              print best
                              putStrLn "Gefilterte Votes nach dem RunOff"
                              print filtered
                              putStrLn "Runoff\n"
                              print runoff

newPath = putStr "Pfad der neuen Wahlen: "
       >> getLine
       >>= \revotes -> putStr "Pfad der neuen Kandidaten: "
       >> getLine
       >>= \candidates -> election revotes candidates


getListOfVoters :: [Voter] -> [Candidate]-> [Voter]
getListOfVoters [] _ = []
getListOfVoters (x:xs) candidates
    | checkVotes (votes x) candidates == False = getListOfVoters xs candidates
    | otherwise = x : getListOfVoters xs candidates

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

getCandidates = atTag "CANDIDATE" >>>
  proc v -> do
    candidate    <- getAttrValue "CANDIDATE" -< v
    returnA -< Candidate candidate




getRunoffCandidates :: Results -> Results
getRunoffCandidates xs
        | null(rlist xs) = xs
        | length(rlist xs) <= 2 = xs
        | otherwise = Results (checkIfSameScore otherCandidates runoffCandidates)
          where
            sorted = sortBy (compare `on` snd) (rlist xs)
            otherCandidates = take (length sorted - 2) sorted
            runoffCandidates = drop (length sorted - 2) sorted


checkIfSameScore :: [(Candidate, Double)] -> [(Candidate, Double)] -> [(Candidate, Double)]
checkIfSameScore [] ys = ys
checkIfSameScore xs [] = []
checkIfSameScore xs (y:ys)
        | null (xs) = (y:ys)
        | snd(last(xs)) == snd(y) = checkIfSameScore (init xs) ((last xs):(y:ys))
        | otherwise = (y:ys)


filterRunoffCandidatesVotes :: [Voter] -> Results -> [Voter]
filterRunoffCandidatesVotes [] _ = []
filterRunoffCandidatesVotes (x:xs) y = [Voter (helpFunctionFORCV (votes x) y)] ++ filterRunoffCandidatesVotes xs y


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
sort (x:xs) = [Voter (sortBy (compare `on` candidate) (votes x))] ++ sort xs



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
