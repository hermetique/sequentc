{-# LANGUAGE FlexibleContexts #-}

module Sequentc (Jmt(..), Rule(..), apply, prove, derive, qed) where

import Data.List
import Control.Monad.State

infix 4 :|-:
data Jmt p = [p] :|-: p deriving (Eq)

infix 3 :---:
data Rule p = [Jmt p] :---: Jmt p deriving (Eq)

-- if the goal rule's first precondition matches up with the step
-- rule's conclusion, we get a rule with the first precondition
-- removed and the step rule's preconditions added in place of it
app :: (Eq p, Show (Rule p)) => Rule p -> Rule p -> Rule p
app s@(step_precs :---: step_conc) g@(goal_precs :---: goal_conc) =
    if step_conc == head(goal_precs)
    then step_precs++(tail(goal_precs)) :---: goal_conc
    else error $ "couldn't apply \n" ++ show s ++ "\n\nto \n\n" ++ show g

prove :: Jmt a -> Rule a
prove j = [j]
          :---:
          j

derive :: (Eq a, Show (Rule a)) => Rule a -> State (Rule a) () -> Rule a
derive goal@(goal_prec :---: goal_conc) proof =
    let proved = snd (runState proof ([goal_conc] :---: goal_conc)) in
    if proved == goal
    then goal
    else error $ "derive not done yet: goal:\n" ++ show goal ++ "\n\n not equal to proved:\n\n" ++ show proved

apply :: (Show (Rule a), Eq a) => Rule a -> State (Rule a) ()
apply r = do
  goal <- get
  put $ app r goal
  return ()

qed :: (Show (Rule a)) => State (Rule a) ()
qed = do
  rule@(goal_precs :---: _) <- get
  case goal_precs of
    [] -> return ()
    _ -> error $ "proof not done yet:\n" ++ show rule

