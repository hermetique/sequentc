import Sequentc
import IL
import Control.Monad.State

a = Const "a"
b = Const "b"
c = Const "c"
gamma = Env "gamma"
delta = Env "delta"

ex1 :: Rule Prop
ex1 = []
      :---:
      [a :=>: b, a] :|-: a :*: b

proof_ex1 = do
  apply $ withEnv ("gamma", [a :=>: b]) $ contraction a (a :*: b)
  apply $ withEnv ("gamma", [a :=>: b, a]) $ withEnv ("delta", [a]) $ (exchg (a :*: b))
  apply $ withEnv ("gamma", [a]) $ withEnv ("delta", [a :=>: b, a]) $ (andI a b)
  apply (idty a)
  apply $ withEnv ("gamma", [a :=>: b]) $ withEnv ("delta", [a]) $ arrE a b
  apply $ idty (a:=>:b)
  apply $ idty a
  qed

-- gamma |- a    gamma |- b
-- ------------------------
-- gamma |- a x b

andI' =
    [[gamma] :|-: a,
     [gamma] :|-: b]
    :---:
    [gamma] :|-: a :*: b

proof_andI' = do
  apply $ withEnv ("gamma", []) $ contraction gamma (a :*: b)
  apply $ withEnv ("delta", [gamma]) $ andI a b
  return ()

ex2 = [gamma] :|-: a :*: b

main = putStrLn  $ show $ map (uncurry derive) [(andI', proof_andI'),
                                                (ex1, proof_ex1)]

