module Codewars.Kata.Tube where
import Codewars.Kata.Tube.Types

-- data Decision = Bus | Walk deriving (Eq, Show)

calculator :: Double   -- ^ the distance you would have to walk
           -> Double   -- ^ the distance the bus would travel
           -> Double   -- ^ the distance you have to *walk* to the bus
           -> Decision -- ^ your decision whether to take the Bus or Walk
calculator distance busDrive busWalk
  | t1 <= 1/6 || t1 <= 2 && t1 <= t2  = Walk
  | otherwise                         = Bus
  where
    t1 = distance / 5
    t2 = busDrive / 8 + busWalk / 5
