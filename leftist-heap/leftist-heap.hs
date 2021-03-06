{-# Language DeriveFoldable #-}

import Prelude hiding (foldl)
import Data.Maybe (fromJust)
import Test.HUnit
import Test.QuickCheck
import Data.Foldable hiding (mapM_)

data Heap a = E | T Int a (Heap a) (Heap a)
  deriving (Show, Eq, Foldable)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge E h = h
merge h E = h
merge h1@(T _ v1 a1 b1) h2@(T _ v2 a2 b2) = if v1 < v2
                                               then makeT v1 a1 (merge b1 h2)
                                               else makeT v2 a2 (merge h1 b2)

  where makeT :: a -> Heap a -> Heap a -> Heap a
        makeT val a b = if rank a >= rank b
                           then T (rank b+1) val a b
                           else T (rank a+1) val b a

rank :: Heap a -> Int
rank E = 0
rank (T r _ _ _) = r

empty :: Heap a
empty = E

insert :: Ord a => Heap a -> a -> Heap a
insert h x = merge (T 1 x E E) h

-- an insert function that directly inserts the new element instead of creating
-- and merging a singleton tree
insert' :: Ord a => Heap a -> a -> Heap a
insert' E x           = T 1 x E E
insert' (T r v E E) x = T r (min v x) (T 1 (max v x) E E) E
insert' (T r v a b) x = T r (min x v) (insert' a (max x v)) b

findMin :: Heap a -> Maybe a
findMin (T _ v _ _) = Just v
findMin E = Nothing

deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = E
deleteMin (T _ _ a b) = merge a b

-- convert a heap to a sorted list
toSortedList :: Ord a => Heap a -> [a]
toSortedList E = []
toSortedList h = fromJust (findMin h):toSortedList (deleteMin h)

tests :: Test
tests = test
  [ "deleteMin empty"  ~:
    (empty :: Heap Int) ~=? deleteMin empty
  , "findMin empty"    ~:
    (Nothing :: Maybe Int) ~=? findMin empty
  , "toSortedList $ findMin [1..10]"  ~:
    [1..10] ~=? toSortedList (foldl insert empty [1..10])
  , "findMin [1..10]"  ~:
    (Just 1::Maybe Int) ~=? findMin (foldl insert empty [1..10])
  , "findMin [10..1]"  ~:
    (Just 1::Maybe Int) ~=? findMin (foldl insert empty [10,9..1])
  , "insert empty 1"   ~:
    (T 1 1 E E :: Heap Int) ~=? insert empty 1
  , "merge empties"    ~:
    (empty :: Heap Int) ~=? merge empty empty
  , "delete + findMin" ~:
    (Just 2 :: Maybe Int) ~=? (findMin . deleteMin . foldl insert empty) [10,9..1]
  ]

prop_insertsEquality :: [Int] -> Bool
prop_insertsEquality xs = let h1 = foldl insert empty xs
                              h2 = foldl insert' empty xs
                           in toSortedList h1 == toSortedList h2

main = do runTestTT tests
          quickCheck prop_insertsEquality
