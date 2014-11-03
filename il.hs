module IL (Prop(..), subs, withEnv, idty, exchg, arrI, arrE, andI, andE, orI1, orI2, orE, contraction, weakening) where

import Sequentc
import ILL
import Data.List

delta = Env "delta"
gamma = Env "gamma"

contraction :: Prop -> Prop -> Rule Prop
contraction a b =
    [([gamma, a, a]) :|-: b]
    :---:
    [gamma, a] :|-: b

--contractEnv :: Prop -> Prop -> Rule Prop
--contractEnv a b =
--    [([gamma, a, a]) :|-: b]
--    :---:
--    [gamma, a] :|-: b


weakening :: Prop -> Prop -> Rule Prop
weakening a b =
    [[gamma] :|-: b]
    :---:
    [gamma, a] :|-: b
