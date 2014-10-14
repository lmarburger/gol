module GoL

-- Using a SortedSet because there is no Set.
import Data.SortedSet

%default total

Coord : Type
Coord = (Int, Int)

world : SortedSet Coord
world = fromList [(0,-1), (0,0), (0,1)]

proximateNeighbors : Coord -> List Coord
proximateNeighbors (x, y) = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                             (x - 1, y    ),             (x + 1, y    ),
                             (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

underConsideration : List Coord -> List Coord
underConsideration []      = []
underConsideration (c::cs) = (proximateNeighbors c) ++ (underConsideration cs)

-- SortedSet.toList $ SortedSet.fromList $ underConsideration $ SortedSet.toList world
