module LearnYouAHaskell.HeathrowToLondon (
    leastExpensiveRouteToLeaf
    , Node(Fork, Next, Leaf)
    , Edge(Edge)
    ) where

data Edge = Edge Int Node deriving Show
data Node = Fork Edge Edge | Next Edge | Leaf deriving Show

recLeastExpensiveRouteToLeaf :: [Edge] -> Node -> [Edge]
recLeastExpensiveRouteToLeaf acc Leaf = reverse acc
recLeastExpensiveRouteToLeaf acc (Next next) =
    recLeastExpensiveRouteToLeaf (next:acc) nextNode
  where (Edge _ nextNode) = next
recLeastExpensiveRouteToLeaf acc (Fork left right)
    | leftWeight >= rightWeight = recLeastExpensiveRouteToLeaf (right:acc) rightNode
    | otherwise = recLeastExpensiveRouteToLeaf (left:acc) leftNode
  where (Edge leftWeight leftNode) = left
        (Edge rightWeight rightNode) = right

leastExpensiveRouteToLeaf :: Node -> [Edge]
leastExpensiveRouteToLeaf = recLeastExpensiveRouteToLeaf []
