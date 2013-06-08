#include <h>

ann :: (a -> b) -> a -> (b, a)
ann f x = (f x, x)

getRegion :: String -> String
getRegion = head . splitOn "-" . last . splitOn ", "

showRegionSet :: (String, [String]) -> [String]
showRegionSet (r, cs) =
    r : cs

abbrNameList :: [(String, String)]
abbrNameList = [
    ("DC", "DC"),
    ("AS", "American Samoa"),
    ("GU", "Guam"),
    ("PR", "Puerto Rico"),
    ("VI", "Virgin Islands"),
    ("MP", "Northern Mariana Islands"),
    ("AK", "Alaska"),
    ("AL", "Alabama"),
    ("AR", "Arkansas"),
    ("AZ", "Arizona"),
    ("CA", "California"),
    ("CO", "Colorado"),
    ("CT", "Connecticut"),
    ("DE", "Delaware"),
    ("FL", "Florida"),
    ("GA", "Georgia"),
    ("HI", "Hawaii"),
    ("IA", "Iowa"),
    ("ID", "Idaho"),
    ("IL", "Illinois"),
    ("IN", "Indiana"),
    ("KS", "Kansas"),
    ("KY", "Kentucky"),
    ("LA", "Louisiana"),
    ("MA", "Massachusetts"),
    ("MD", "Maryland"),
    ("ME", "Maine"),
    ("MI", "Michigan"),
    ("MN", "Minnesota"),
    ("MO", "Missouri"),
    ("MS", "Mississippi"),
    ("MT", "Montana"),
    ("NC", "North Carolina"),
    ("ND", "North Dakota"),
    ("NE", "Nebraska"),
    ("NH", "New Hampshire"),
    ("NJ", "New Jersey"),
    ("NM", "New Mexico"),
    ("NV", "Nevada"),
    ("NY", "New York"),
    ("OH", "Ohio"),
    ("OK", "Oklahoma"),
    ("OR", "Oregon"),
    ("PA", "Pennsylvania"),
    ("RI", "Rhode Island"),
    ("SC", "South Carolina"),
    ("SD", "South Dakota"),
    ("TN", "Tennessee"),
    ("TX", "Texas"),
    ("UT", "Utah"),
    ("VA", "Virginia"),
    ("VT", "Vermont"),
    ("WA", "Washington"),
    ("WI", "Wisconsin"),
    ("WV", "West Virginia"),
    ("WY", "Wyoming")]

nameAbbrMap :: Map.Map String String
nameAbbrMap = Map.fromList $ map swap abbrNameList

nameToAbbr :: String -> String
nameToAbbr k = fromMaybe (error k) $ Map.lookup k nameAbbrMap

abbrToNameMap :: Map.Map String String
abbrToNameMap = Map.fromList abbrNameList

abbrIfNot :: String -> String
abbrIfNot "N. Marianas" = "MP"
abbrIfNot k = Map.findWithDefault k k nameAbbrMap

main :: IO ()
main = do
    abbrToUasMap <-
        Map.fromList .
        map (first abbrIfNot) .
        map (\xs -> (fst $ head xs, map snd xs)) .
        map (take 5) . groupBy ((==) `on` fst) . map (ann getRegion) .
        lines <$> getContents
    abbrToCapMap <- 
        Map.fromList .
        map (first nameToAbbr) .
        map (second tail . break (== '\t')) .
        lines <$> readFile "/home/danl/Anki/uscaps.txt"
    let abbrToNameCapMap =
            Map.unionWith (++)
            (Map.map (:[]) abbrToNameMap)
            (Map.map (:[]) abbrToCapMap)
    let abbrToAllMap =
            Map.unionWith (++)
            abbrToNameCapMap
            abbrToUasMap
    putStr . unlines . map (\ (abbr, l) -> abbr ++ "\t" ++ l) .
        Map.toList . Map.map (\ x -> "\"" ++ x ++ "\"") $
        Map.map (intercalate "<BR>") abbrToAllMap
