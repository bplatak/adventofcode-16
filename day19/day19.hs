-- Solve the josephus problem
solve :: Int -> Int
solve n = 1+2*n-2^(1+(floor(lg)))
    where lg = log(fromIntegral n)/log(2.0)

-- Solve part 2 
solve2 :: Int -> Int 
solve2 n | fromIntegral dst < mid = dst+1
         | otherwise              = 2*dst-mid+1
    where grp = ceiling(log(fromIntegral n)/log(3.0))
          dst = n-(1+3^(grp-1))
          mid = 3^(grp-1)

main = do 
    print $ solve  69
    print $ solve2 3017957