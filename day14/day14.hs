import qualified Data.ByteString.Char8 as BS 
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as BS16

type HashMeta = (Maybe Char, [Char])

-- Generate an infinite list of MD5 hashes 
hashes :: Int -> String -> [String]
hashes n salt = map (foldr (.) id $ replicate n hash) $ strings 0
    where strings idx = (salt ++ (show idx)):strings (idx+1)
          hash        = BS.unpack . BS16.encode . MD5.hash . BS.pack

-- Get the first triplet 
triplet :: String -> Maybe Char 
triplet (a:b:c:rest) | a==b && b==c = Just a
                     | otherwise    = triplet (b:c:rest)
triplet _            = Nothing

-- Get all 5-tuplets 
quintuplets :: String -> [Char]
quintuplets (a:rest) | cmp1 == cmp2 = [a] ++ quintuplets (drop 4 rest)
                     | otherwise    = quintuplets rest 
    where (cmp1, cmp2) = (replicate 4 a, take 4 rest)
quintuplets _        = []

-- Map hash to meta 
meta :: String -> HashMeta 
meta hash = (triplet hash, quintuplets hash)

-- Check if a hash is a key
isKey :: [HashMeta] -> (Int -> Bool)
isKey hashes idx = let (tri, quin) = hashes!!idx in case tri of 
    Just c  -> (any (elem c) . map snd . take 999 . drop (idx+1)) hashes  
    Nothing -> False 

main = do 
    let hashes1     = hashes 1      "cuanljph" 
    let hashes2017  = hashes 2017   "cuanljph" 

    print $ (filter (isKey $ map meta $ hashes1) [0..])!!63
    print $ (filter (isKey $ map meta $ hashes2017) [0..])!!63