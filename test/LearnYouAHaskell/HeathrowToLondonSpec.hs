module LearnYouAHaskell.HeathrowToLondonSpec (spec) where

import Test.Hspec (describe, it, shouldBe)
import LearnYouAHaskell.HeathrowToLondon (
    leastExpensiveRouteToLeaf
    , Edge(Edge)
    , Node(Fork, Next, Leaf)
    )

makeEdges :: [Int] -> (Edge, Edge)
makeEdges (x:y:0:[]) = (Edge x Leaf, Edge y Leaf)
makeEdges (x:y:z:xs) = (left, right)
  where (leftEdge, rightEdge) = makeEdges xs
        left = Edge x (Fork leftEdge (Edge z (Next rightEdge)))
        right = Edge y (Fork (Edge z (Next leftEdge)) rightEdge)

makeStartingNode :: [Int] -> Node
makeStartingNode weights = Fork left right
  where (left, right) = makeEdges weights

heathrowToLondon = makeStartingNode [50, 10, 30, 5, 90, 20, 40, 2, 25, 10, 8, 0]

getWeight :: Edge -> Int
getWeight (Edge weight i) = weight

weights :: [Edge] -> [Int]
weights = map getWeight

spec =
  describe "fastestRoute" $ do
    it "solves the Heathrow to London map" $
      let edgesToLeaf = leastExpensiveRouteToLeaf heathrowToLondon
      in
        weights edgesToLeaf `shouldBe` [10, 30, 5, 20, 2, 8]
