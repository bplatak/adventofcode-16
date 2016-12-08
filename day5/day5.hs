import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map
import Data.Char
import Data.Maybe

type Generator = Integer -> String
type DigitMap  = Map.Map Integer Integer

-- Compute hash
computeHash :: String -> Generator
computeHash id idx = (show . md5 . B.pack) (id ++ (show idx))

-- Check if hash is valid
isHashValid :: String -> Bool
isHashValid ('0':'0':'0':'0':'0':_) = True
isHashValid _                       = False

-- Find next idx
findIdx :: Generator -> (Integer -> Integer)
findIdx gen idx | isHashValid $ gen idx = idx
                | otherwise             = findIdx gen (idx + 1)

-- Find the password
password :: Generator -> [Integer]
password gen = (findIdx gen 0):map ((findIdx gen).(+1)) (password gen)

-- Find the more complex password
findPassword :: [String] -> DigitMap -> [Char]
findPassword (pass:rest) digits
                    | length digits == 8     = map (intToDigit.fromInteger.fromJust.(`Map.lookup` digits)) [0..7]
                    | pos > 7                = findPassword rest digits
                    | Map.member pos digits  = findPassword rest digits
                    | otherwise              = findPassword rest (Map.insert pos digit digits)
    where
        digit = toInteger (digitToInt (pass!!6))
        pos   = toInteger (digitToInt (pass!!5))


main = do
    let gen  = computeHash "abc"
    let keys = password gen

    print $ map ((!!5).gen) $ take 8 keys
    print $ findPassword (map gen keys) Map.empty