-- Generate given amount of data using modified dragon curve and initial state
modifiedDragonCurve :: [Bool] -> Int -> [Bool]
modifiedDragonCurve s x = take x $ (iterate f s)!!idx
    where f s     = s ++ [False] ++ (map not . reverse) s 
          (a, x') = (fromIntegral $ length s, fromIntegral x)
          idx     = ceiling $ logBase 2 ((x'+1) / (a+1))

-- An infinite list solution (http://stackoverflow.com/questions/41183330/infinite-self-referencing-list)
-- This seems to require WAY too much memory, but it does feel a lot nicer.
-- modifiedDragonCurve :: [Bool] -> [Bool]
-- modifiedDragonCurve s = s ++ concatMap f [0..]
--     where f n   = False : (map not . reverse) (take (len n) $ modifiedDragonCurve s)
--           len n = (2^n)*(length s+1)-1

-- Generate checksum 
checksum :: [Bool] -> [Bool]
checksum dta | odd $ length csum = csum  
             | otherwise         = checksum csum
    where f (a:b:rest) | a==b = [True] ++ f rest
                       | a/=b = [False] ++ f rest
          f []         = []
          csum         = f dta 

-- Convert int <-> bool
boolToInt a = case a of True -> 1; False -> 0;
intToBool a = case a of 1 -> True; 0 -> False;

main = do 
    -- Solve the first part 
    let part1 = map intToBool [0,0,1,0,1,0,0,0,1,0,1,1,1,1,0,1,0]

    print $ concat $ map (show . boolToInt) $ checksum $ modifiedDragonCurve part1 272
    print $ concat $ map (show . boolToInt) $ checksum $ modifiedDragonCurve part1 35651584

