import Data.Function (on)
import Data.List (sortBy)

data Wahlobjekt = Wahlobjekt {
    bezeichnung   :: String,
    wahlErgebnis  :: Double
  } deriving (Show)

data Voter = Voter {
  vote :: [(String,Double)]
} deriving (Show)

kandidat1 = Wahlobjekt "kandidat1" 0
kandidat2 = Wahlobjekt "kandidat2" 0
kandidat3 = Wahlobjekt "kandidat3" 0
kandidat4 = Wahlobjekt "kandidat4" 0
kandidat5 = Wahlobjekt "kandidat5" 0
kandidat6 = Wahlobjekt "kandidat6" 0
listeKandidaten = [kandidat1,kandidat2,kandidat3,kandidat4,kandidat5,kandidat6]

voter1=Voter [("kandidat1",3),("kandidat2",2),("kandidat3",1),("kandidat4",5),("kandidat5",2),("kandidat6",0)]
voter2=Voter [("kandidat1",1),("kandidat2",4),("kandidat3",1),("kandidat4",5),("kandidat5",2),("kandidat6",0)]
voter3=Voter [("kandidat1",1),("kandidat2",4),("kandidat3",1),("kandidat4",5),("kandidat5",2),("kandidat6",0)]
voter4=Voter [("kandidat1",1),("kandidat2",4),("kandidat3",1),("kandidat4",5),("kandidat5",2),("kandidat6",0)]
voter5=Voter [("kandidat1",1),("kandidat2",4),("kandidat3",1),("kandidat4",5),("kandidat5",2),("kandidat6",0)]
voter6=Voter [("kandidat1",1),("kandidat2",4),("kandidat3",1),("kandidat4",5),("kandidat5",2),("kandidat6",0)]
listeVoter = [voter1,voter2,voter3,voter4,voter5,voter6]


filteredVoter :: Voter -> Wahlobjekt -> Voter
filteredVoter voter wahlobjekt = Voter (filter ((==bezeichnung wahlobjekt).fst) (vote voter))


getRating :: [Voter] -> Wahlobjekt -> Double
getRating (x:xs) kandidat
        | null (x:xs) = 0
        | length (x:xs) == 1 = if (fst(head(vote (filteredVoter x kandidat))) == (bezeichnung kandidat)) then snd(head(vote (filteredVoter x kandidat))) else 0
        | fst(head(vote (filteredVoter x kandidat))) == (bezeichnung kandidat) = snd(head(vote (filteredVoter x kandidat))) + (getRating xs kandidat)
        | otherwise = getRating xs kandidat


getResults :: [Wahlobjekt] -> [Voter] -> [Wahlobjekt]
getResults (x:xs) voters
        | null (x:xs) = []
        | length (x:xs) == 1 = (Wahlobjekt (bezeichnung x) (getRating voters x)) : []
        | otherwise = (Wahlobjekt (bezeichnung x) (getRating voters x)) : getResults xs voters


getTwoHighest :: [Wahlobjekt] -> [Wahlobjekt]
getTwoHighest xs
        | null xs = []
        | length(xs) <= 2 = xs
        | otherwise = drop (length sorted - 2) sorted
          where
            sorted = sortBy (compare `on` wahlErgebnis) xs




-- ZUM Testen : "getResults listeKandidaten listeVoter" in Konsole eingeben, gibt eine Liste mit den Wahlobjekten und Ergebnissen zurück

-- fst(head(vote vote1)) um ersten Eintrag vom Tumpel zu bekommen
-- snd(head(vote vote1)) um zweiten Eintrag vom Tumpel zu bekommen


-- https://stackoverflow.com/questions/14955627/shorthand-way-for-assigning-a-single-field-in-a-record-while-copying-the-rest-o
