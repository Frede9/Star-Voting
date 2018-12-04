data Wahlobjekt = Wahlobjekt {
    bezeichnung   :: String,
    wahlErgebnis  :: Double
  } deriving (Show)

data Voter = Voter {
  vote :: [(String,Double)]
} deriving (Show)

-- fst(head(vote vote1)) um ersten Eintrag vom Tumpel zu bekommen
-- snd(head(vote vote1)) um zweiten Eintrag vom Tumpel zu bekommen


-- https://stackoverflow.com/questions/14955627/shorthand-way-for-assigning-a-single-field-in-a-record-while-copying-the-rest-o
