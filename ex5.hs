import Data.Char

main = do
    print $ length $ react "dabAcCaCBAcCcaDA"
    print $ (react "dabAcCaCBAcCcaDA" == "dabCBAcaDA")
    s <- readFile "input5.txt"
    let t = filter (/= '\n') s
    print $ length $ react $  t
    print $ minimum $ map (length . react) $ map (removeletter t) alphabet


react :: String -> String         
react xs = if reaction xs == xs then xs
           else react $ reaction xs


reaction :: String -> String
reaction [] = []
reaction [x] = [x]
reaction (x:y:xs) = if (x /= y) && (toUpper x == toUpper y) 
                    then reaction xs 
                    else x:(reaction (y:xs))


alphabet = "abcdefghijklmnopqrstuvwxyz"
removeletter st c = filter (/= toLower c) $ filter (/= toUpper c) st