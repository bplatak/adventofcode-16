import Data.List.Split (splitOn)

test1 = "ADVENT"
test2 = "A(1x5)BC"
test3 = "(3x3)XYZ"
test4 = "A(2x2)BCD(2x2)EFG"
test5 = "(6x1)(1x3)A"
test6 = "X(8x2)(3x3)ABCY"
test7 = "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"

-- Compute the decompressed length of a string 
decompressedLength :: String -> Int 
decompressedLength ('(':rest) = (cmd!!1)*decompressedLength repe + decompressedLength post
    where cmd  = map read $ splitOn "x" $ takeWhile (/=')') rest 
          repe = take (cmd!!0) $ drop 1 $ dropWhile (/=')') rest 
          post = drop ((cmd!!0)+1) $ dropWhile (/=')') rest
decompressedLength (a:rest) = 1 + decompressedLength rest
decompressedLength []       = 0

main = do
    strings <- lines <$> readFile "day9/data/data.in"
    let solver1 = foldl (+) 0 . map decompressedLength

    print $ solver1 strings