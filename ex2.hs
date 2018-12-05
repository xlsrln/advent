

import Data.List

count x xs = length [ e | e <- xs, e == x]
freqs xs = [count x xs | x <- nub xs ]

check2 xs = if elem 2 $ freqs xs then 1 else 0
check3 xs = if elem 3 $ freqs xs then 1 else 0

checksum ls = (sum $ map check2 ls) * (sum $ map check3 ls)

main = do
    t <- readFile "input2.txt"
    let x = lines t
    let lista =  [(a,b) | a <- x, b <- x, a /= b]
    let svar = [(a,b) | (a,b) <- lista, compare1 a b]
    let a = fst $ head svar :: String
    let b = snd $ head svar :: String
    print $ checksum x
    print $ overlap a b



compare1 [] [] = True
compare1 (a:as) (b:bs) = if (a==b) then (compare1 as bs) else (secondchance as bs)
    where 
        secondchance (a:as) (b:bs) = if (a==b) then secondchance as bs else False
        secondchance [] [] = True


overlap (a:as) (b:bs) = if a == b then a:(overlap as bs) else (overlap as bs)
overlap [] [] = []