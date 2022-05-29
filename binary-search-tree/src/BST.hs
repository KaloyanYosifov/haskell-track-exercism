module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where
import           Data.Foldable (foldl')

data BST a = Empty | Node {
            left   :: BST a
            ,val   :: a
            ,right :: BST a
        }
    deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty        = Nothing
bstLeft (Node l _ _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Empty        = Nothing
bstRight (Node _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Empty        = Nothing
bstValue (Node _ v _) = Just v

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldl' (\acc x -> insert x acc) empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node empty x empty
insert x node@(Node l v r)
  | x > v = Node l v $ insert x r
  | x <= v = Node (insert x l) v r
  | otherwise = node

singleton :: a -> BST a
singleton x = Node empty x empty

toList :: BST a -> [a]
toList Empty        = []
toList (Node l v r) = toList l ++ [v] ++ toList r
