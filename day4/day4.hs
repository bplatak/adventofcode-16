import Data.List
import Data.List.Split
import Data.Char

type Name       = [String]
type Id         = Int
type Checksum   = String
type Room       = (Name, Id, Checksum)

room = parseLine "aaaaa-bbb-z-y-x-123[abxyz]"
room2 = parseLine "totally-real-room-200[decoy]"

-- Parse the input line onto a room type
parseLine :: String -> Room
parseLine line = (roomName, roomId, roomChecksum)
    where
        roomDef         = splitOn "-" $ takeWhile (/='[') line
        roomName        = init roomDef
        roomId          = read $ last roomDef
        roomChecksum    = tail $ init $ dropWhile (/='[') line

-- Compute a checksum of a room
type CharCount = (Char, Int)
checksumCount :: [CharCount] -> String -> [CharCount]
checksumCount counts []  = counts
checksumCount counts str = checksumCount (counts++[(char, count)]) rem
    where
        char  = str !! 0
        count = length $ takeWhile (==char) str
        rem   = dropWhile (==char) str

-- Sort chars
sortChar :: CharCount -> CharCount -> Ordering
sortChar (_, a) (_, b) = compare a b

-- Compute a checksum
checksum :: Name -> Checksum
checksum name = map (\(a,b) -> a) $ take 5 sorted
    where
        sorted = reverse $ sortBy sortChar $ reverse (checksumCount [] $ sort $ concat name)

-- solve
solve :: [Room] -> Int
solve rooms = sum $ map (\(_,i,_) -> i) $ filter (\(n,_,c) -> c == checksum n) rooms

-- Decode character
decodeChar :: Int -> Char  -> Char
decodeChar _ '-'    = ' '
decodeChar key char = chr ((((ord char) + key - 97) `mod` 26) + 97)

-- Decode room
decodeRoom :: Room -> (String, Id)
decodeRoom (name, id, _) = (map (decodeChar id) $ concat name, id)

main = do
    fileData <- readFile "day4/data/data.in"
    let problem = map parseLine $ lines fileData


    print $ map (decodeRoom) problem