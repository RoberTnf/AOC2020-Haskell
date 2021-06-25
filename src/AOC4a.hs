module AOC4a where

import AOC
import Data.List.Split
import qualified Data.Map as Map
import Text.Regex.TDFA

necessaryFields :: [String]
necessaryFields = ["byr","iyr", "eyr","hgt","hcl","ecl","pid"]

type Document = Map.Map String String

getRegexGroups :: String -> [[String]]
getRegexGroups s = s =~ "([a-zA-Z]+):([a-zA-Z#0-9]+)"

-- |
-- >>> intoDocument "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"
-- fromList [("ecl","brn"),("eyr","2025"),("hcl","#cfa07d"),("hgt","59in"),("iyr","2011"),("pid","166559648")]
intoDocument :: String -> Document
intoDocument x =
    let groupsToDocuments [_, k, v] = (k, v)
        groupsToDocuments _ = error "Malformed input"
    in Map.fromList . map groupsToDocuments . getRegexGroups $ x

formatInput :: [String] -> [String]
formatInput = splitOn "\n\n" . unlines

-- |
-- >>> let input = lines "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm"
-- >>> map (isValid . intoDocument) . formatInput $ input
-- [True,False,True]
isValid :: Document -> Bool
isValid doc =
    let isKey x = not . null . Map.lookup x $ doc
    in all isKey necessaryFields


aoc4a :: IO ()
aoc4a = do
  input <- getInput 4
  print . length . filter (isValid . intoDocument) . formatInput $ input
--   print . length . formatInput $ input