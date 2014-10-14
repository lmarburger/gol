module GoL

-- Square
--
-- > SortedSet.toList square
-- [(0, 0), (0, 1), (1, 0), (1, 1)] : List (Int, Int)
-- > SortedSet.toList $ nextWorld square
-- [(0, 0), (0, 1), (1, 0), (1, 1)] : List (Int, Int)
-- > SortedSet.toList $ nextWorld $ nextWorld square
-- [(0, 0), (0, 1), (1, 0), (1, 1)] : List (Int, Int)

-- Flip flop
--
-- > SortedSet.toList world
-- [(0, -1), (0, 0), (0, 1)] : List (Int, Int)
--
-- > SortedSet.toList $ nextWorld world
-- [(-1, 0), (0, 0), (1, 0)] : List (Int, Int)
--
-- > SortedSet.toList $ nextWorld $ nextWorld world
-- [(0, -1), (0, 0), (0, 1)] : List (Int, Int)
--
-- > SortedSet.toList $ nextWorld $ nextWorld $ nextWorld world
-- [(-1, 0), (0, 0), (1, 0)] : List (Int, Int)

-- Using a SortedSet because there is no Set.
import Data.SortedSet

%default total

Coord : Type
Coord = (Int, Int)

neighbors : Coord -> List Coord
neighbors (x, y) = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                    (x - 1, y    ),             (x + 1, y    ),
                    (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

intersection : Eq a => List a -> List a -> List a
intersection []      bs = []
intersection (a::as) bs =
  if elem a bs then
    a :: intersection as bs
  else
    intersection as bs

coordScore : Coord -> SortedSet Coord -> Nat
coordScore c world = length $ intersection (SortedSet.toList world) $ neighbors c

underConsideration : List Coord -> List Coord
underConsideration []      = []
underConsideration (c::cs) = (neighbors c) ++ (underConsideration cs)

survives : Coord -> SortedSet Coord -> Bool
survives c world = if score == 3 || (score == 2 && alive)
                   then True
                   else False
  where
    score : Nat
    score = coordScore c world

    alive : Bool
    alive = (List.elem c $ SortedSet.toList world)

nextWorld : SortedSet Coord -> SortedSet Coord
nextWorld world = SortedSet.fromList $
                  foldl (\survivors => (\coord => if survives coord world
                                                  then coord :: survivors
                                                  else survivors)) [] $
                  SortedSet.toList $ SortedSet.fromList $ -- remove duplicates
                  underConsideration $
                  SortedSet.toList world

flipflop : SortedSet Coord
flipflop = fromList [(0,-1), (0,0), (0,1)]

square : SortedSet Coord
square = fromList [(0,0), (1,0), (0,1), (1,1)]
