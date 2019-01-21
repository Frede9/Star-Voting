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
  voters <- runX (parseXML votesxml >>> getVoters)
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

-- Ausgabe von allen Daten
printing best filtered runoff = do
                              print best
                              putStrLn "Gefilterte Votes nach dem RunOff"
                              print filtered
                              putStrLn "Runoff\n"
                              print runoff

-- Eingabe der neuen Pfade von Wahlen sowie Kandidaten
newPath = putStr "Pfad der neuen Wahlen: "
       >> getLine
       >>= \revotes -> putStr "Pfad der neuen Kandidaten: "
       >> getLine
       >>= \candidates -> election revotes candidates

-- Eingabeparameter: Erste Liste von allen Votern, Liste von Kandidaten
-- Ausgabeparameter: Liste von Votern mit korrekten Votes
-- Zweck: Ausgabe der Voter mit den korrekten Votes mit Hilfsmethode checkVotes
getListOfVoters :: [Voter] -> [Candidate]-> [Voter]
getListOfVoters [] _ = []
getListOfVoters (x:xs) candidates
    | checkVotes (votes x) candidates == False = getListOfVoters xs candidates
    | otherwise = x : getListOfVoters xs candidates

-- Eingabeparameter: Liste von Votes eines Voters, Liste von den Kandidaten
-- Ausgabeparameter: Ausgabe ob die Votes korrekt sind
-- Zweck: Überprüfen der Votes eines Voters 
checkVotes :: [Vote] -> [Candidate] -> Bool
checkVotes [] [] = True
checkVotes x [] = False
checkVotes [] y = False
checkVotes (x:xs) candidates
     | stars x > 5 || stars x < 0 = False
     | (elem) (candidate x) (candidates) = checkVotes (xs) (filter  (not.(==(candidate x))) candidates)
     | otherwise = False

-- Eingabeparameter: Liste von Votern mit korrekten Votes, Map zur Berechnung der Votes
-- Ausgabeparameter: Liste von den Kandidaten mit der Gesamtbewertung
-- Zweck: Berechnen und Ausgabe der Kandidaten+Bewertungen mit der Methode insertVotes
getResults :: [Voter] -> Map Candidate Double -> [(Candidate, Double)]
getResults [] mymap = Map.toList mymap
getResults (x:xs) mymap = getResults xs (insertVotes (votes x) (mymap))

-- Eingabeparameter: Liste von Votes eines Voters zum Einfügen in die Map, Map zur Berechnung der Votes
-- Ausgabeparameter: Map mit den Kandidaten+Bewertung eines Voters
-- Zweck: Addieren der einzelnen Bewertungen für einen bestimmten Kandidaten mit Hilfe einer Map
insertVotes :: [Vote] -> Map Candidate Double -> Map Candidate Double
insertVotes [] mymap = mymap
insertVotes (x:xs) mymap = insertVotes xs (Map.insertWith (+) (candidate x) (stars x) mymap)

-- Einlesen und Parsen der XML-Datei
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file
                             
-- Eingabeparameter: gesuchter tag in der XML-Datei
-- Ausgabeparameter: Ausgabe der Elemente des tags
-- Zweck: Durchqueren und Selektieren der Knoten mit bestimmtem tag
atTag tag = deep (isElem >>> hasName tag)

-- Zweck: Parsen der Votes in der XML-Datei in Objekte vom Typ Vote
-- Durch proc werden die einzelnen tags durchsucht
-- und anschließend returned
getVotes = atTag "VOTE" >>>
  proc v -> do
    candidate    <- getAttrValue "CANDIDATE" -< v
    vote    <- getAttrValue  "RATING"    -< v
    returnA -< Vote {candidate = Candidate candidate, stars = read vote::Double}

-- Zweck: Parsen der Voter in der XML-Datei in Objekte vom Typ Voter
-- Voter haben nur eine Liste von Votes. Daher werden über getVotes die
-- einzelnen Votes geholt und anschließend die Voter returned
getVoters = atTag "VOTER" >>>
  proc vr -> do
    votes <- listA getVotes    -< vr
    returnA -< Voter {votes = votes}

-- Zweck: Parsen der Candidates in der XML-Datei in Objekte vom Typ Candidate
getCandidates = atTag "CANDIDATE" >>>
  proc c -> do
    candidate    <- getAttrValue "CANDIDATE" -< c
    returnA -< Candidate candidate

-- Eingabeparameter: Results
-- Ausgabeparameter: Results
-- Zweck: Kontrolle, ob weniger als zwei Kandidaten vorhanden sind und weiterleiten an Hilfsfunktion
-- Where: Sortierung der Liste, damit die letzten beiden Einträge(höchste Punktzahl) und der Rest getrennt werden kann
getRunoffCandidates :: Results -> Results
getRunoffCandidates xs
        | null(rlist xs) = xs
        | length(rlist xs) <= 2 = xs
        | otherwise = Results (checkIfSameScore otherCandidates runoffCandidates)
          where
            sorted = sortBy (compare `on` snd) (rlist xs)
            otherCandidates = take (length sorted - 2) sorted
            runoffCandidates = drop (length sorted - 2) sorted

-- Eingabeparameter: rlist von Result & rlist von Result bzw. jetzige RunoffKandidaten und restlichen Kandidaten
-- Ausgabeparameter: rlist von Result mit den wirklichen Runoffkanidaten ob 2 oder mehr
-- Zweck: Kontrolle, ob der letzte Eintrag der restlichen Kandidaten und der erste Runoff Kandidat dieselbe Punktzahl hat
-- Wenn ja, dann hinzufügen und neu aufrufen, wenn nein, dann Rückgabe der RunoffKandidaten
checkIfSameScore :: [(Candidate, Double)] -> [(Candidate, Double)] -> [(Candidate, Double)]
checkIfSameScore [] ys = ys
checkIfSameScore xs [] = []
checkIfSameScore xs (y:ys)
        | null (xs) = (y:ys)
        | snd(last(xs)) == snd(y) = checkIfSameScore (init xs) ((last xs):(y:ys))
        | otherwise = (y:ys)


-- Eingabeparameter: Liste der Voter und Runoff Kandidaten(Laenge 2) 
-- Ausgabeparameter: Liste der gefilterten Voter
-- Zweck: Erstellen einer Liste und weiterleiten an Hilfsfunktion, primär zum zerlegen der Liste gedacht(Uebersichtlicher)
filterRunoffCandidatesVotes :: [Voter] -> Results -> [Voter]
filterRunoffCandidatesVotes [] _ = []
filterRunoffCandidatesVotes (x:xs) y = [Voter (helpFunctionFORCV (votes x) y)] ++ filterRunoffCandidatesVotes xs y


-- Eingabeparameter: Liste der Votes und Runoff Kandidaten(Laenge 2) 
-- Ausgabeparameter: Liste der gefilterten Votes
-- Zweck: Nur die Votes mit den jeweiligen Kandidaten aus den Results sollen übrig bleiben um die weitere Auswertung zu erleichtern
helpFunctionFORCV :: [Vote] -> Results -> [Vote]
helpFunctionFORCV [] _ = []
helpFunctionFORCV (x:xs) y
  | firstCandidate == (candidate x) = x : helpFunctionFORCV xs y
  | secondCandidate == (candidate x) = x : helpFunctionFORCV xs y
  | otherwise  = helpFunctionFORCV xs y
    where
      firstCandidate = fst(head(rlist y))
      secondCandidate = fst(last(rlist y))

-- Eingabeparameter: Liste der Voter
-- Ausgabeparameter: Liste der sortierten voter
-- Zweck: Alle einzelnen Glieder der Voter werden intern anhand des Kandidaten Names sortiert
sort :: [Voter] -> [Voter]
sort [] = []
sort (x:xs) = [Voter (sortBy (compare `on` candidate) (votes x))] ++ sort xs



-- Eingabeparameter: rlist von Results
-- Ausgabeparameter: rlist von Results mit den berechneten Ergebnissen
-- Zweck: Weiterleiten an Hilfsfunktion um die Liste aufzuspalten
doRunoff :: [(Candidate, Double)] -> [Voter] -> [(Candidate, Double)]
doRunoff [] _ = []
doRunoff (x:xs) ys =  [(canidateName,countWins canidateName ys )] ++ doRunoff xs ys
  where
    canidateName = fst(x)


-- Eingabeparameter: Einzelner Kandidat und die Liste der Voter
-- Ausgabeparameter: Double Wert wie häufig der eine Kandidat dem anderen Kandidaten ueberlegen war
-- Zweck: Mehrere Vergleiche um herauszufinden welcher Kandidat hoeher gewertet worden ist pro Waehler
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
