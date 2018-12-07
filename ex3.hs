import Data.List

main = do 
    t <- readFile "input3.txt"
    let x = lines t
    let labels = map (fst . head . reads . tail) x :: [Int]
    let sizes = map (tuplemaker . reads) $ (map last) $ (map words) x
    let places = map (tuplemaker . reads . droplast) $ (map secondlast) $ (map words) x
    let xmax = 1 + (maximum $ fst $ unzip sizes) + (maximum $ fst $ unzip places)
    let ymax = 1 + (maximum $ snd $ unzip sizes) + (maximum $ snd $ unzip places)
    let aa = mxnull xmax ymax
    let inputs = makeinput places sizes
    let result = addinputs aa inputs
    let failures = [ x | x <- concat result, x > 1]
    print $ length failures
    let answer = snd $ head [x | x <- map (findoverlap inputs) inputs, fst x == 0]
    print $ labels !! (head $ elemIndices answer inputs)

--adds list of inputs iteratively
addinputs mx [] = mx
addinputs mx (y:inputs) = addinputs (addb mx y) inputs

--adds a whole box
addb xs (a,b,c,d) = addbox xs a b c d 0
    where
        addbox [] x y wx wy iter = []
        addbox (row:mx) x y wx wy iter 
            | iter < y 
                = row : addbox mx x y wx wy (iter+1)
            | (iter > y-1) && (iter < y + wy)     
                = (addboxrow row x wx 0) : addbox mx x y wx wy (iter+1)
            | iter+1 > (y + wy)                      
                = row : addbox mx x y wx wy (iter+1)

--adds a 1-dim box slice to a row
addboxrow [] start len iter = []
addboxrow (el:row) start len iter
    | iter < start 
        = el  : addboxrow row start len (iter+1)
    | (iter > start-1) && (iter < start + len) 
        = (el+1) : addboxrow row start len (iter+1)
    | iter+1 > (start + len) 
        = el : addboxrow row start len (iter+1)

--minor help functions
makeinput [] [] = []
makeinput (x:xs) (y:ys) = (fst x, snd x, fst y, snd y) : (makeinput xs ys)

tuplemaker (x:xs) = (fst x, read $ tail $ snd x) :: (Int, Int)

secondlast xs = last $ droplast xs

droplast xs = reverse $ tail $ reverse xs

mxnull n m = replicate n (replicate m 0)

compareinputs (a1,b1,w1,h1) (a2,b2,w2,h2) = ( compare1 (a1,w1) (a2,w2) ) * ( compare1 (b1,h1) (b2,h2) )
    where compare1 (x, y) (z, w)
            | x >= z && x < z+w = 1
            | z >= x && z < x+y = 1
            | otherwise = 0
        
findoverlap inputs bx = ( maximum [ compareinputs a bx | a <- inputs, a /= bx] , bx)
