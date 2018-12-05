fixnum xs = [ x | x <- xs, not (x `elem` "+")]

first = (\(a,b,c,d) -> a)
second = (\(a,b,c,d) -> b)
third = (\(a,b,c,d) -> c)
fourth = (\(a,b,c,d) -> d)

rp (xs, (y:ys),b,n) = ((y+head xs):xs, reverse (y:(reverse ys)),elem (y+head xs) xs,n+1)

findrep (ys, xs, b, n) = if b then head ys
                         else if n < 100000000 then findrep $ rp (ys,xs,b,n) else 0

main = do 
    t <- readFile "input.txt"
    let x = map read (lines $ fixnum t) :: [Integer]
    print $ sum (map read (lines $ fixnum t) :: [Integer])
    print $ findrep ([0],x,False,0)
