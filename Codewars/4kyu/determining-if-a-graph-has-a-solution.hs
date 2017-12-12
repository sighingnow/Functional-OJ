module Graph where

import Data.List

type Node = Char
type Arc  = (Node, Node)

dfs :: [Node] -> Node -> Node -> [Arc] -> (Bool, [Node])
dfs visited s e arcs
  | elem s visited = (False, visited)
  | otherwise =
        foldl' (\(r, vs) (_, y) ->
            if r || y == e
               then (True, vs)
               else dfs vs y e arcs)
        (False, (s:visited))
        (filter (\(x, _) -> x == s) arcs)

solveGraph :: Node -> Node -> [Arc] -> Bool
solveGraph s e arcs
  | s == e = True
  | otherwise = fst $ dfs [] s e arcs
