import Data.List.Split (splitOn)

-- Decompression type length
data DecompressionType = V1 | V2 deriving Eq 

-- Compute the decompressed length of a string 
decompressedLength :: DecompressionType -> String -> Int 
decompressedLength V1 ('(':rest) = c*l + (decompressedLength V1 postseq)
    where (l,c,w) = parseMarker rest 
          postseq = drop (l+w) rest 
decompressedLength V2 ('(':rest) = c*(decompressedLength V2 subseq) + (decompressedLength V2 postseq)
    where (l,c,w) = parseMarker rest 
          postseq = drop (l+w) rest 
          subseq  = take l $ drop w rest 
decompressedLength v (a:rest)      = 1 + decompressedLength v rest
decompressedLength _ []            = 0

-- Parse the marker 
parseMarker :: String -> (Int, Int, Int)
parseMarker input = (l,c,length str + 1)
    where str   = takeWhile (/=')') input 
          [l,c] = map (read::String->Int) $ splitOn "x" str

main = do
    strings <- lines <$> readFile "day9/data/data.in"
    let solver1 = foldl (+) 0 . map (decompressedLength V1)
    let solver2 = foldl (+) 0 . map (decompressedLength V2)

    print $ solver1 strings
    print $ solver2 strings