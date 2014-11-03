import Sequentc
import IL
import Control.Monad.State

a = Const "a"
b = Const "b"
c = Const "c"

ex1 :: Jmt Prop
ex1 = [a :=>: b, a] :|-: a :*: b

test :: State (Rule Prop) ()
test = do
  apply (contraction [a :=>: b] a (a :*: b))
  apply (exchg [a :=>: b, a] [a] (a :*: b))
  apply (andI [a] a [a :=>: b, a] b)
  apply (idty a)
  apply (arrE [a :=>: b] [a] a b)
  apply (idty (a :=>: b))
  apply (idty a)
  qed

main = putStrLn  $ show $ snd $ runState test (prove ex1)
