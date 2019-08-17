module MyHaskell where


-- Dries Janse
-- r0627054
-- Schakelprogramma Master toegepaste Informatica



-- Task 1a

data Circuit
  = INPUT String | NOT Circuit | AND Circuit Circuit  | OR Circuit Circuit | XOR Circuit Circuit
    deriving (Eq)
-- Task 1b

cinput :: String -> Circuit
cinput = INPUT

cnot   :: Circuit -> Circuit
cnot   = NOT

cand   :: Circuit -> Circuit -> Circuit
cand   = AND

cor    :: Circuit -> Circuit -> Circuit
cor    = OR

cxor   :: Circuit -> Circuit -> Circuit
cxor   = XOR

-- Task 1c

example :: Circuit
example = cor (cand (cinput "x") (cinput "y"))   (cxor  ((cnot (cinput "z"))) (cinput "x") )

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany ((AND c1 c2):cs) = (AND c1 c2)
candMany (c:cs) = candMany cs



-- Task 2a

instance Show Circuit where
  show (INPUT s)    = s
  show (NOT c1)     = "NOT(" ++ show c1 ++ ")"
  show (AND c1 c2)  = "AND(" ++ show c1 ++ "," ++ show c2 ++ ")"
  show (OR c1 c2)   = "OR(" ++ show c1 ++ "," ++ show c2 ++ ")"
  show (XOR c1 c2)  = "XOR(" ++ show c1 ++ "," ++ show c2 ++ ")"

-- Task 2b

simplify :: Circuit -> Circuit
simplify (INPUT s)   = INPUT s
simplify (AND s1 s2) = AND (simplify s1) (simplify s2)
simplify (NOT s)     = NOT (simplify s)
simplify (XOR s1 s2) = simplify (OR (AND (simplify s1) (NOT (simplify s2)))  (AND (NOT (simplify s1)) (simplify s2)))
simplify (OR  s1 s2) = NOT (AND (NOT (simplify s1)) (NOT(simplify s2)))
--
-- Task 2c

size :: Circuit -> Int
size (INPUT s)   = 0
size (NOT s)     = 1 + (size s)
size (AND s1 s2) = 1 + (size s1) + (size s2)
size (XOR s1 s2) = 1 + (size s1) + (size s2)
size (OR s1 s2)  = 1 + (size s1) + (size s2)

-- Task 2d

gateDelay :: Circuit -> Int
gateDelay (INPUT s)   = 0
gateDelay (NOT s)     = 1 + gateDelay s
gateDelay (AND s1 s2) = 1 + (max (gateDelay s1) (gateDelay s2))
gateDelay (XOR s1 s2) = 1 + (max (gateDelay s1) (gateDelay s2))
gateDelay (OR s1 s2)  = 1 + (max (gateDelay s1) (gateDelay s2))

-- Task 2e

inputs :: Circuit -> [String]
inputs c = allInputsWithDubbels c []


allInputsWithDubbels:: Circuit ->  [String]-> [String]
allInputsWithDubbels (INPUT s)  cs   | (elem s cs) = cs
                                     | otherwise   =  cs ++ [s]
allInputsWithDubbels (NOT s)   cs   = allInputsWithDubbels s cs
allInputsWithDubbels (AND s1 s2) cs = allInputsWithDubbels (s2) (allInputsWithDubbels s1 cs)
allInputsWithDubbels (XOR s1 s2) cs = allInputsWithDubbels (s2) (allInputsWithDubbels s1 cs)
allInputsWithDubbels (OR s1 s2)  cs = allInputsWithDubbels (s2) (allInputsWithDubbels s1 cs)

-- Task 3a

simulate :: Circuit -> [(String,Bool)] -> Bool
simulate (INPUT s) xs   = getValueForString s xs
simulate (NOT s)  xs    =  (not (simulate s xs))
simulate (AND s1 s2) xs =  (simulate s1 xs) && (simulate s2 xs)
simulate (XOR s1 s2) xs =  ((simulate s1 xs) && (not (simulate s2 xs))) ||  ((not(simulate s1 xs)) && (simulate s2 xs))
simulate (OR s1 s2)  xs =  (simulate s1 xs) || (simulate s2 xs)


getValueForString :: String -> [(String,Bool)] ->Bool
getValueForString as ((s,b):xs) | as == s = b
                                | otherwise = getValueForString as xs
getValueForString _ [] = False
--Als deze niet gevonden kan worden

-- Task 3b

combinations :: Int -> [[Bool]]
combinations 0 = [[]]
--combinations 1 = [[True],[False]]
combinations n = combinationsHelper 1 n [[False],[True]]


combinationsHelper:: Int -> Int ->  [[Bool]] -> [[Bool]]
combinationsHelper actual target xs | actual == target = xs
                                    | actual < target  = combinationsHelper (actual+1) target newCombination
                                        where newCombination = (addFalseToEveryCombination xs) ++ (addTrueToEveryCombination xs)


addTrueToEveryCombination:: [[Bool]] -> [[Bool]]
addTrueToEveryCombination [] = []
addTrueToEveryCombination (x:xs) = [[True]++x] ++ (addTrueToEveryCombination xs)

addFalseToEveryCombination:: [[Bool]] -> [[Bool]]
addFalseToEveryCombination [] = []
addFalseToEveryCombination (x:xs) = [[False]++x] ++ (addFalseToEveryCombination xs)


combinationsToInteger:: [[Bool]] -> [[Int]] -> [[Int]]
combinationsToInteger [] x =x
combinationsToInteger (b:bs) x = combinationsToInteger (bs) (x++[boolArrToIn b])

boolArrToIn [] =  []
boolArrToIn (b:bs)    | b = [1] ++ (boolArrToIn bs)
                      | otherwise = [0] ++ (boolArrToIn bs)
-- Task 3c
tabulate :: Circuit -> IO ()
tabulate c = do (printAllVariables (inputs c))
                putStrLn "| outputs"
                printallPossibilities icom results
         where bcom = combinations (length (inputs c))
               icom = combinationsToInteger bcom []
               valcom = map (zip (inputs c)) (bcom)
               results = boolArrToIn (map (simulate c) valcom)

--alle outputs te genereren map (zip )
printAllVariables :: [String] -> IO()
printAllVariables [] = return ()
printAllVariables (x:xs) = do putStr (x ++ " ")
                              printAllVariables xs

printAllValues:: [Int] -> IO()
printAllValues [] = return ()
printAllValues (x:xs) = do putStr ((show x) ++ " ")
                           printAllValues xs


printallPossibilities :: [[Int]] -> [Int] -> IO ()
printallPossibilities _ []  = return ()
printallPossibilities (val:vals) (result:results) = do printAllValues val
                                                       putStrLn ("| " ++ (show result))
                                                       printallPossibilities vals results
