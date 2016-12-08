import Data.List (transpose, sort, minimumBy, maximumBy, group)

-- Parse file
parseFile :: String -> [[Char]]
parseFile = lines

---- Decode message
type Sorter = (CharCount -> CharCount -> Ordering) -> [CharCount] -> CharCount
decodeMessage :: Sorter -> [[Char]] -> [Char]
decodeMessage sorter = map (chr . (sorter cmp) . computeCharCounts . sort) . transpose
    where cmp = (\(_,a) (_,b) -> compare a b)
          chr = (\(c,_)       -> c)


type CharCount = (Char, Int)
computeCharCounts :: [Char] -> [CharCount]
computeCharCounts []          = []
computeCharCounts (char:rest) = [(char, len)] ++ computeCharCounts (drop (len - 1) rest)
    where len = ((length rest) + 1) - (length $ dropWhile (==char) rest)

test = ["eedadn","drvtee","eandsr","raavrd","atevrs","tsrnev","sdttsa","rasrtv","nssdts","ntnada","svetve","tesnvt","vntsnd","vrdear","dvrsen","enarar"]





main = do
    fileData <- readFile "day6/data/data.in"
    let input = parseFile fileData
    print $ decodeMessage maximumBy input
    print $ decodeMessage minimumBy input