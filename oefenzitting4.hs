import Control.Applicative -- backwards compatibility

import Control.Monad

data Identity a = Identity a

  deriving (Eq, Ord, Show)



data Pair a b = Pair a b
 
 deriving (Eq, Ord, Show)



data Unit a = Unit
 deriving (Eq, Ord, Show)


instance Functor Identity where
  fmap f (Identity x) = Identity (f x)


instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Functor Unit where
 fmap f Unit = Unit

sumABC:: [(String,Int)] -> Maybe Int
sumABC xs = case (lookup "A" xs) of
                  Nothing -> Nothing
                  Just a -> case (lookup "B" xs) of
                                Nothing -> Nothing
                                Just b -> case (lookup "C" xs) of
                                            Nothing -> Nothing
                                            Just c -> Just (a+b+c)


sumABCBind :: [(String,Int)] -> Maybe Int
sumABCBind xs = (lookup "A" xs) >>= \a ->
                (lookup "B" xs) >>= \b ->
                (lookup "C" xs) >>= \c ->
                return (a+b+c)

sumABCDo :: [(String,Int)] -> Maybe Int
sumABCDo xs = do a <- (lookup "A" xs)
                 b <- (lookup "B" xs)
                 c <- (lookup "C" xs)
                 return (a+b+c)
