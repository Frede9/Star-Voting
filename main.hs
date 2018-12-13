data Wahlobjekt = Wahlobjekt {
    bezeichnung   :: String,
    wahlErgebnis  :: Double
  } deriving (Show)

data Voter = Voter {
  vote :: [(String,Double)]
} deriving (Show)


type VoterList = [Voter]


kandidat1 = Wahlobjekt "kandidat1" 0
kandidat2 = Wahlobjekt "kandidat2" 0
listeKandidaten = [kandidat1,kandidat2]

voter1=Voter [("kandidat1",3),("kandidat2",2)]
voter2=Voter [("kandidat1",1),("kandidat2",4)]
listeVoter = [voter1,voter2]


--getVoter[] = []
--getVoter(x:xs) ys = getKanidat x ys
--getKandidat x [] = 0
--getKandidat x (y:ys)
-- | fst(head(vote x)) == (bezeichnung y) =  add x y
-- | otherwise = getKandidat x ys


-- add x y = snd(head(vote x)) + (wahlErgebnis y)

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

-- ZUM Testen : "getResults listeKandidaten listeVoter" in Konsole eingeben, gibt eine Liste mit den Wahlobjekten und Ergebnissen zurück

-- fst(head(vote vote1)) um ersten Eintrag vom Tumpel zu bekommen
-- snd(head(vote vote1)) um zweiten Eintrag vom Tumpel zu bekommen


-- https://stackoverflow.com/questions/14955627/shorthand-way-for-assigning-a-single-field-in-a-record-while-copying-the-rest-o
