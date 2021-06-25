-- Things got a little more difficult here. Lots of small issues with regexp, that I added into the doctests.
-- Also had some issues dealing with Lookup and its Maybe Bool type, solved it by using the justAndTrue function.

module AOC4b where

import AOC
import Data.Char
import Data.List.Split
import qualified Data.Map as Map
import Text.Regex.TDFA

type Document = Map.Map String String

necessaryFields :: [String]
necessaryFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

functionMap :: Map.Map String (String -> Bool)
functionMap = Map.fromList [("byr", byr), ("iyr", iyr), ("eyr", eyr), ("hgt", hgt), ("hcl", hcl), ("ecl", ecl), ("pid", pid)]

fourDigits :: String -> Bool
fourDigits x = x =~ "^[0-9]{4}$"

-- |
-- >>> byr "01932"
-- False
-- >>> byr "1920"
-- True
byr :: String -> Bool
byr x = fourDigits x && read x >= 1920 && read x <= 2002

iyr :: String -> Bool
iyr x = fourDigits x && read x >= 2010 && read x <= 2020

eyr :: String -> Bool
eyr x = fourDigits x && read x >= 2020 && read x <= 2030

-- |
-- >>> hgt "153cm"
-- True
--
-- >>> hgt "72in"
-- True
--
-- >>> hgt "72inasda"
-- False
--
-- >>> hgt "99in"
-- False
hgt :: String -> Bool
hgt x =
  let match = x =~ "^([0-9]+)(in|cm)$" :: [[String]]
   in (not . null $ match) && isValidHeight match

isValidHeight :: [[String]] -> Bool
isValidHeight [[_, h, unit]] = case unit of
  "cm" -> read h >= 150 && read h <= 193
  "in" -> read h >= 59 && read h <= 76
  _ -> False
isValidHeight _ = False

-- |
-- >>> hcl "#123abc"
-- True
--
-- >>> hcl "#123abk"
-- False
--
-- >>> hcl "#123abca"
-- False
hcl :: String -> Bool
hcl x = x =~ "^#[0-9a-f]{6}$"

pid :: String -> Bool
pid x = x =~ "^[0-9]{9}$"

ecl :: String -> Bool
ecl x = x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

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

isValid :: Document -> Bool
isValid doc =
  let keyValue x = Map.lookup x doc
      isKey x = not . null $ keyValue x
      keyValid x = Map.lookup x functionMap <*> keyValue x
      s x = map keyValid necessaryFields
   in all isKey necessaryFields && all (justAndTrue . keyValid) necessaryFields

justAndTrue :: Maybe Bool -> Bool
justAndTrue (Just x) = x
justAndTrue Nothing = False

-- |
-- >>> let invalidInput = lines "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\niyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946\n\nhcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\nhgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007"
-- >>> length . filter (isValid . intoDocument) . formatInput $ invalidInput
-- 0
--
-- >>> let validInput = lines "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f\n\neyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\nhcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022\n\niyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719\n"
-- >>> length . filter (isValid . intoDocument) . formatInput $ validInput
-- 4
aoc4b :: IO ()
aoc4b = do
  input <- getInput 4
  print . length . filter (isValid . intoDocument) . formatInput $ input
