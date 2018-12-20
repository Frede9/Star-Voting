import Data.Function (on)
import Data.List (sortBy)

data VoteObject = VoteObject {
    name   :: String,
    voteResult  :: Double
  } deriving (Show)

data Voter = Voter {
  vote :: [(String,Double)]
} deriving (Show)

kandidat1 = VoteObject "kandidat1" 0
kandidat2 = VoteObject "kandidat2" 0
kandidat3 = VoteObject "kandidat3" 0
kandidat4 = VoteObject "kandidat4" 0
kandidat5 = VoteObject "kandidat5" 0
kandidat6 = VoteObject "kandidat6" 0
listeKandidaten = [kandidat1,kandidat2,kandidat3,kandidat4,kandidat5,kandidat6]

voter1=Voter [("kandidat1",1),("kandidat2",3),("kandidat3",2),("kandidat4",3),("kandidat5",4),("kandidat6",0)]
voter2=Voter [("kandidat1",1),("kandidat2",1),("kandidat3",1),("kandidat4",1),("kandidat5",1),("kandidat6",0)]
voter3=Voter [("kandidat1",1),("kandidat2",1),("kandidat3",1),("kandidat4",1),("kandidat5",1),("kandidat6",0)]
voter4=Voter [("kandidat1",1),("kandidat2",1),("kandidat3",1),("kandidat4",1),("kandidat5",1),("kandidat6",0)]
voter5=Voter [("kandidat1",1),("kandidat2",1),("kandidat3",1),("kandidat4",1),("kandidat5",1),("kandidat6",0)]
voter6=Voter [("kandidat1",1),("kandidat2",1),("kandidat3",1),("kandidat4",1),("kandidat5",1),("kandidat6",0)]
listeVoter = [voter1,voter2,voter3,voter4,voter5,voter6]


getResults :: [VoteObject] -> [Voter] -> [VoteObject]
getResults (x:xs) voters
        | null (x:xs) = []
        | length (x:xs) == 1 = (VoteObject (name x) (getRating voters x)) : []
        | otherwise = (VoteObject (name x) (getRating voters x)) : getResults xs voters

getRating :: [Voter] -> VoteObject -> Double
getRating (x:xs) kandidat
        | null (x:xs) = 0
        | length (x:xs) == 1 = if (fst(head(vote (filteredVoter x kandidat))) == (name kandidat)) then snd(head(vote (filteredVoter x kandidat))) else 0
        | fst(head(vote (filteredVoter x kandidat))) == (name kandidat) = snd(head(vote (filteredVoter x kandidat))) + (getRating xs kandidat)
        | otherwise = getRating xs kandidat

filteredVoter :: Voter -> VoteObject -> Voter
filteredVoter voter voteObject = Voter (filter ((==name voteObject).fst) (vote voter))


getRunoffCanidates :: [VoteObject] -> [VoteObject]
getRunoffCanidates xs
        | null xs = []
        | length(xs) <= 2 = xs
        | otherwise = checkIfSameScore otherCanidates runoffCanidates
          where
            sorted = sortBy (compare `on` voteResult) xs
            otherCanidates = take (length sorted - 2) sorted
            runoffCanidates = drop (length sorted - 2) sorted

checkIfSameScore :: [VoteObject] -> [VoteObject] -> [VoteObject]
checkIfSameScore [] ys = ys
checkIfSameScore xs [] = []
checkIfSameScore xs (y:ys)
      | null (xs) = (y:ys)
      | newCanidateResult == runoffCanidateResult = checkIfSameScore (init xs) ((last xs):(y:ys))
      | otherwise = (y:ys)
        where
          newCanidateResult = voteResult (last xs)
          runoffCanidateResult = voteResult y



--doRunoff :: [Wahlobjekt] -> [Voter]-> Wahlobjekt
--doRunoff xs voters
--      |
--      |
--      |




-- ZUM Testen : "getResults listeKandidaten listeVoter" in Konsole eingeben, gibt eine Liste mit den Wahlobjekten und Ergebnissen zurück
-- getRunoffCanidates wird mit der Liste aus getResults gefüttert und gibt alle Kanidaten wieder, die den Score höchsten Score besitzen



-- fst(head(vote vote1)) um ersten Eintrag vom Tumpel zu bekommen
-- snd(head(vote vote1)) um zweiten Eintrag vom Tumpel zu bekommen
-- https://stackoverflow.com/questions/14955627/shorthand-way-for-assigning-a-single-field-in-a-record-while-copying-the-rest-o
