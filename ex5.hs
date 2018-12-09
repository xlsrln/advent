import Data.Char

main = do
    test
    s <- readFile "input5.txt"          
    let t = filter (/= '\n') s
    print $ length $ react $  t
    print $ minimum $ map (length . react) $ map (removeletter t) alphabet

test = do
    let teststring = "dabAcCaCBAcCcaDA" 
    let testanswer = react teststring
    print $ length $ testanswer   
    print $ (testanswer == "dabCBAcaDA") 


-- iterate reaction until nothing happens
react :: String -> String         
react xs = if reaction xs == xs then xs
           else react $ reaction xs

-- go through string and make all reactions happen
reaction :: String -> String
reaction [] = []
reaction [x] = [x]
reaction (x:y:xs) = if (x /= y) && (toUpper x == toUpper y) 
                    then reaction xs 
                    else x:(reaction (y:xs))

-- other stuff needed
alphabet = "abcdefghijklmnopqrstuvwxyz"
removeletter st c = filter (/= toLower c) $ filter (/= toUpper c) st
