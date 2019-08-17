import Data.Char
import Control.Monad

data MyBool = MyTrue | MyFalse
data Exp = Const MyBool
           | And Exp Exp
           | Or Exp Exp

instance Eq MyBool where
 MyTrue  == MyTrue  = True
 MyFalse == MyFalse = True
 _       == _       = False

instance Eq Exp where
 (Const x)      == (Const y)     = x == y
 (And ex1 ex2) == (And ex3 ex4) = ex1 == ex3 && ex2 == ex4
 (Or ex1 ex2)  == (Or ex3 ex4)  = ex1 == ex3 && ex2 == ex4
 _             == _             = False


instance Show MyBool where
  show MyTrue  = "True"
  show MyFalse = "False"

instance Show Exp where 
   show (Const x)   = show x
   show (And e1 e2) = show e1 ++ " && " ++ show e2
   show (Or  e1 e2) = show e1 ++ " || " ++ show e2

class Evaluatable a where
  eval :: a -> Bool

instance Evaluatable MyBool where
 eval MyTrue  = True
 eval MyFalse = False

instance Evaluatable Exp where
 eval (Const x)     = eval x
 eval (And e1 e2)   = eval e1 && eval e2
 eval (Or e1 e2)    = eval e1 || eval e2

class Sequence a where
 next:: a -> a
 prev:: a -> a

--ord char -> Int
--chr Int -> Char

class Sequence a => LeftBoundedSequence a where
   firstElem:: a

class Sequence a => RightBounedSequence a where
   lastElem:: a


prog1:: IO()
prog1 = do m <-getLine
           n <-getLine
           replicateM_ (read m) (putStrLn n)


prog1b:: IO()
prog1b = getLine >>= \m ->
        getLine >>= \n ->
        replicateM_ (read m) (putStrLn n)

prog2:: IO()
prog2 = do st <- getLine
           prog2Helper st
           


prog2Helper:: String -> IO()
prog2Helper "" =  return ()
prog2Helper s  = do putStrLn (reverse s)
                    prog2 



index::[IO a] -> IO Int -> IO a
index ls ii = do x <- ii
                 ls !! x

inc :: (Show b, Num b) => b -> IO b
inc x = do let y = x+1
           print y
           return y

dec x = do 
       let y = x-1
       print y
       return y


loop:: a -> (a -> IO a) -> IO()
loop st op = do x <- op st
                loop x op



menu:: [(String, a -> IO a)] -> a -> IO a
menu list nbr = do
                 putStrLn "---"
                 putStrLn "Menu"
                 putStrLn "---"
                 putStrLn ""
                 printMenu list 1
                 putStrLn ""
                 putStr "==>"
                 inp <-getLine
                 (snd (list !! ((read inp)-1))) nbr
                 
                 




printMenu:: [(String, a -> IO a)] -> Int -> IO ()
printMenu [] i         = return ()
printMenu ((x,_):xs) i = do putStrLn ((show i) ++". " ++ x)
                            printMenu xs (i+1)




 