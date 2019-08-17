count:: [Int] -> Int
count [] = 0
count(x:xs) = 1 + (count xs)

myAnd:: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && myAnd xs


myOr:: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

append:: [Int] -> [Int] -> [Int]
append [] x = x
append (x:xs) y = x : (append xs y)

myProduct:: [Integer] -> Integer
myProduct []    = 1
myProduct (x:xs)= x * myProduct xs

insert:: Int -> [Int] -> [Int]
insert x []     = [x]
insert x (y:ys) | x <= y    = x:y:ys
                | otherwise = y : (insert x ys)

myLast:: [Int] -> Int
myLast [x]    = x
myLast (x:xs) = myLast xs

data Name = N String
    deriving (Show, Eq)

data Pair = Pa Int Int
    deriving (Show, Eq)

data Gender = Male | Female | Other
    deriving (Show, Eq)

data Person = Per Name Int Gender
    deriving (Show, Eq)

data TestResult = Pass Int | Fail [String]
    deriving (Show, Eq)

stringToGender:: String -> Gender
stringToGender s | s == "Male" = Male
                 | s == "Female" = Female
                 | otherwise = Other


genderToString:: Gender -> String
genderToString g = show g

passing::Int -> TestResult
passing = Pass

failing:: [String] -> TestResult
failing = Fail


grade::TestResult -> Int
grade (Pass x) = x
grade (Fail _) = 0



comments::TestResult -> [String]
comments (Pass _ ) = []
comments (Fail x) = x


data Move = Rock | Paper | Scissors
       deriving (Show, Eq)

beat::Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock 


lose::Move -> Move
lose Rock     = Scissors
lose Paper    = Paper
lose Scissors = Rock


data Result = Win | Lose | Draw

outcome::Move -> Move -> Result
outcome p1 p2 | p1 == p2        = Draw
              | (lose p1) == p2 = Win
              | otherwise = Lose


factorial::Integer-> Integer
factorial n | n < 1 = 1
            | otherwise = product [1..n]


myRepeat::Int -> Int -> [Int]
myRepeat x y | x > 0 = [ y | z <- [1..x]]
             | otherwise = []



flatten:: [[Int]] -> [Int]
flatten x = [ y | z <- x, y <-z]


range::Int -> Int -> [Int]
range x y = [z | x < y, z <-[x..y]]


sumInts:: Int -> Int -> Int
sumInts x y | y >= x = sum [x..y]
            | otherwise = 0

removeMultiples::Int -> [Int] -> [Int]
removeMultiples x ls = [ l | l<-ls, (mod l x) ==1]




data Exp = Const Int
 
           | Add Exp Exp
           | Sub Exp Exp
           | Mul Exp Exp
      deriving(Show, Eq)

eval::Exp -> Int
eval (Const x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y

data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

type Prog  = [Inst]
type Stack = [Int]

runtimeError:: Stack
runtimeError = error "Runtime error."

execute:: Inst -> Stack -> Stack
execute (IPush x) y = x:y
execute IAdd (x1:x2:xs) = x1+x2:xs
execute IMul (x1:x2:xs) = x1*x2:xs
execute ISub (x1:x2:xs) = x2-x1:xs
execute _ _ = runtimeError
           

run::Prog -> Stack -> Stack
run [] x     = x
run (x:xs) y = (run xs (execute x y))

-- approxiating pi

sumf::[Float] -> Float
sumf = foldr (+) 0 

productf::[Float] -> Float
productf= foldr (*) 1


piSum::Float -> Float
piSum x = 8.0 * sumf [ 1/(((4*z)+1)*((4*z)+3))  | z <-  [0..x]]

piProd::Float -> Float
piProd x = 4* productf [ ((2*n+2)*(2*n+4))/((2*n+3)**2) | n <- [0..x]] 



sieve::Int -> [Int]
sieve x | x<3 = []
        | otherwise = sieveDeleter [2..(x-1)] 0 x


sieveDeleter :: [Int] -> Int ->Int -> [Int]
sieveDeleter xs index input | ((length(xs)-1) < index) || (floorSquare (xs !! index)) > input    = xs
                            | otherwise = sieveDeleter (delete xs index) (index+1) input
                                  where delete xs index = [ x | x<-xs, (xs !! index)==x || mod x (xs !!index) /= 0]  


--[ y | y<-ys, y==x || not( (mod y x) == 0)]  


sqrtMono :: Double -> Double

sqrtMono = sqrt



i2d :: Int -> Double

i2d = fromIntegral



floorMono :: Double -> Int

floorMono = floor


-- -------------------------



floorSquare :: Int -> Int

floorSquare = floorMono . sqrtMono . i2d


















