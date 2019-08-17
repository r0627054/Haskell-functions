import Data.Char

import Data.List


mySum::[Integer] -> Integer
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct:: [Integer] -> Integer
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

foldints op ini []    = ini
foldints op ini (x:xs)= op x (foldints op ini xs)


myFoldl op ini [] = ini
myFoldl op ini (x:xs) = myFoldl op (op ini x) xs


readInBase:: Int -> [Int] -> Int
readInBase base xs = myFoldl (\acc x -> (acc*base) + x) 0 xs



myMap:: (a ->b) ->[a] -> [b]
myMap f ls = [ f l  | l<-ls] 

myMapf:: (a ->b) ->[a] -> [b]
myMapf f ls = foldr (\c r -> f c:r) [] ls

applyAll:: [a->a] -> a -> a
applyAll [] a =a
applyAll (x:xs) a = x $ (applyAll xs a)

applyTimes::Int -> (a->a) -> a -> a
applyTimes 0 func start = start
applyTimes times func start = applyAll [func | x <- [1..times]] start

applyMultipleFuncs::a -> [a -> b] -> [b]
applyMultipleFuncs start funcs = [ f start | f<- funcs]



mapLC:: (a->b) -> [a] -> [b]
mapLC func list = [func el | el<-list]

filterLC::(a-> Bool) -> [a] -> [a]
filterLC pred list = [ el | el<-list, pred el]



odds::[Int]
odds = [2*x+1 | x<-[0..]]


isOdd:: Int -> Bool
isOdd x = mod x 2 /=0

pythagorean::[(Int,Int,Int)]
pythagorean = [ ((m^2)-(n^2),2*n*m,(m^2)+(n^2)) | m <-[1..], n<-[1..m], (gcd m n) == 1, not( isOdd m && isOdd n)]

partialSums:: Num a => [a] -> [a]
partialSums xs = partialSumsHelper xs 0


partialSumsHelper:: Num a => [a] -> a -> [a]
partialSumsHelper [] a =[a]
partialSumsHelper (x:xs) prev = x+prev : partialSumsHelper xs (x+prev)




merge::Ord a => [a] -> [a] -> [a]
merge [] a = a
merge a [] = a
merge (x:xs) (y:ys) | x == y = x : merge xs ys
                    | x < y  = x : merge xs (y:ys)
                    | otherwise =  y : merge (x:xs) ys




hamming :: [Integer]

hamming = 1 : map (2*) hamming `merge`

              map (3*) hamming `merge`

              map (5*) hamming


row::[Integer] -> [Integer]
row x = zipWith (+) (0:x) (x++[0])


pascal::[[Integer]]
pascal= [1] : map row pascal



bincoeff:: Int -> Int -> Integer
bincoeff x y = pascal !! x !! y








--Caesar Cipher 

let2int :: Char -> Int

let2int c = ord c - ord 'a'



int2let :: Int -> Char

int2let n = chr (ord 'a' + n)



shift::Int -> Char -> Char
shift i c | let2int c >= 0 && let2int c < 26 = int2let (abs(mod ((let2int c)+i) 26))
          | otherwise = c

 
encode::Int -> String -> String
encode fact s = map (\x -> shift fact x) s

percent::Int -> Int -> Float
percent x y = fromIntegral x *100 / fromIntegral y

freqs :: String -> [Float]

freqs ss = [percent (count x) tot | x <- ['a'..'z']]

  where count c = length(filter (==c) ss)
       
        tot     = length(filter isLower ss)


chisqr:: [Float] -> [Float] -> Float
chisqr o e = sum [ (oi-ei)^2/ei |(oi,ei)<-pairs]
     where pairs = zip o e

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs


table :: [Float]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4
        ,
 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]



shiftFactor str = [chisqr (rotate n table') table | n <- [0..25]]
      where table' = freqs str
































