module IL (Prop(..), subs, idty, exchg, arrI, arrE, andI, andE, orI1, orI2, orE, contraction, weakening) where

import Sequentc
import ILL
import Data.List

contraction :: [Prop] -> Prop -> Prop -> Rule Prop
contraction gamma a b = [(gamma ++ [a, a]) :|-: b]
              :---:
              (gamma ++ [a]) :|-: b

weakening :: [Prop] -> Prop -> Prop -> Rule Prop
weakening env a b = [env :|-: b]
                :---:
                env ++ [a] :|-: b
