import Data.List.Split (splitOn)

test1 = "ADVENT"
test2 = "A(1x5)BC"
test3 = "(3x3)XYZ"
test4 = "A(2x2)BCD(2x2)EFG"
test5 = "(6x1)(1x3)A"
test6 = "X(8x2)(3x3)ABCY"

-- Compute the decompressed length of a string 
decompressedLength :: String -> Int 
decompressedLength ('(':rest) = (cmd!!1)*(cmd!!0) + decompressedLength post
    where cmd  = map read $ splitOn "x" $ takeWhile (/=')') rest 
          post = drop ((cmd!!0)+1) $ dropWhile (/=')') rest
decompressedLength (a:rest) = 1 + decompressedLength rest
decompressedLength []       = 0

main = do
    strings <- lines <$> readFile "day9/data/data.in"
    let solver1 = foldl (+) 0 . map decompressedLength

    print $ solver1 strings