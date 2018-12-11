data Wahlobjekt = Wahlobjekt {
    bezeichnung   :: String,
    wahlErgebnis  :: Double
  } deriving (Show)

data Voter = Voter {
  vote :: [(String,Double)]
} deriving (Show)



kanidat1 = Wahlobjekt "kanidat1" 0
kanidat2 = Wahlobjekt "kanidat2" 0
listeKanidaten = [kanidat1,kanidat2]

voter1=Voter [("kanidat1",3),("kanidat1",2)]
voter2=Voter [("kanidat1",1),("kanidat2",4)]
listeVoter = [voter1,voter2]


--getVoter[] = []
--getVoter(x:xs) ys = getKanidat x ys
getKanidat x [] = 0
getKanidat x (y:ys)
 | fst(head(vote x)) == (bezeichnung y) =  add x y
 | otherwise = getKanidat x ys


add x y = snd(head(vote x)) + (wahlErgebnis y)




-- fst(head(vote vote1)) um ersten Eintrag vom Tumpel zu bekommen
-- snd(head(vote vote1)) um zweiten Eintrag vom Tumpel zu bekommen


-- https://stackoverflow.com/questions/14955627/shorthand-way-for-assigning-a-single-field-in-a-record-while-copying-the-rest-o
