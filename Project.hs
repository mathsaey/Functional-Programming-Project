{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,FlexibleInstances, FlexibleContexts #-}

-- DEBUG
import Graph.BinarySearchTree
import Graph.Dijkstra


t0 = foldl (\acc x -> insertNode acc x) (empty :: (BSTGraph Int Int)) [1..5]
t1 = insertEdge t0 (1,2) 1
t2 = insertEdge t1 (2,3) 1
t3 = insertEdge t2 (3,4) 1
t4 = insertEdge t3 (4,5) 1
t5 = insertEdge t4 (1,5) 2

x = t4
y = t5
-- END DEBUG