----------------------------------------
-- |
-- Module    : Data.InfiniteSet
-- License   : MIT
--
-- Maintainer  : Joomy Korkut <cumhurkorkut@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- An experimental infinite set implementation.
-- Important: This structure can contain repeated elements in a set.
-- This contradicts the essence of sets, but since herbrand-prolog
-- doesn't use delete, we can ignore this issue, because
-- checking for repeated elements is too costly.
-- Especially in infinite sets, things go crazy pretty quickly.
----------------------------------------

module Data.InfiniteSet
where

data Set a = Set   [a]
           | Union [Set a]
           deriving Eq

-- Query

-- | O(1). Is this the empty set?
null :: Set a -> Bool
null (Set   []) = True
null (Union []) = True
null _        = False

-- | The number of elements in the set.
-- Will not terminate if the set is infinite, use at your own risk.
size :: Set a -> Int
size (Set   xs) = length xs
size (Union xs) = sum $ map size xs

-- | Is the element in the set?
-- If the set is infinite and the element is not found,
-- it will never terminate, use at your own risk.
member :: Eq a => a -> Set a -> Bool
member x (Set   xs) = x `elem` xs
member x (Union xs) = undefined -- TODO


-- | Is the element not in the set?
-- If the set is infinite and the element is not found,
-- it will never terminate, use at your own risk.
notMember :: Eq a => a -> Set a -> Bool
notMember x s = not $ member x s

-- Construction

-- | O(1). The empty set.
empty :: Set a
empty = Set []

-- | O(1). Create a singleton set.
singleton :: a -> Set a
singleton x = Set [x]

-- | Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.
insert :: a -> Set a -> Set a
insert x (Set   xs)     = Set (x:xs)
insert x (Union [])     = singleton x
insert x (Union (y:ys)) = Union $ insert x y : ys

-- | Delete an element from a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.
delete :: Eq a => a -> Set a -> Set a
delete x (Set   xs) = Set $ filter (== x) xs
delete x (Union xs) = Union $ map (delete x) xs

-- Combine

-- | The union of two sets.
union :: Set a -> Set a -> Set a
union x@(Set xs)   y@(Set ys)   = Union [x, y]
union x@(Set xs)   y@(Union ys) = Union (x:ys)
union x@(Union xs) y@(Set ys)   = y `union` x
union x@(Union xs) y@(Union ys) = Union [x, y] -- this might change

-- | The union of a list of sets.
unions :: [Set a] -> Set a
unions = foldl union empty

-- Conversion

-- | Convert the set to a list of elements.
toList :: Set a -> [a]
toList (Set xs) = xs
toList (Union xs) = concatMap toList xs

-- | Create a set from a list of elements.
fromList :: Eq a => [a] -> Set a
fromList = Set
