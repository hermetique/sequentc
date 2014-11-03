{-# LANGUAGE FlexibleInstances #-}
module ILL (Prop(..), subs, idty, exchg, arrI, arrE, andI, andE, orI1, orI2, orE) where

import Sequentc

import Data.List

infix 7 :*:
infix 7 :+:
infix 6 :=>:
data Prop =
    Const String | 
    Prop :*: Prop |
    Prop :+: Prop |
    Prop :=>: Prop deriving (Eq)

instance Subst (Rule Prop) where
    subs from to (precs :---: conc) = (map (subs from to) precs)
                                      :---:
                                      (subs from to conc)

rename :: String -> String -> Rule Prop -> Rule Prop
rename from to r = subs (Const from) (Const to) r

idty :: Prop -> Rule Prop
idty a =
    []
    :---:
    [a] :|-: a

exchg :: [Prop] -> [Prop] -> Prop -> Rule Prop
exchg gamma delta a = [delta ++ gamma :|-: a]
                      :---:
                      gamma ++ delta :|-: a

arrI :: [Prop] -> Prop -> Prop -> Rule Prop
arrI env a b =
    [env ++ [a] :|-: b]
    :---:
    env :|-: a :=>: b

arrE :: [Prop] -> [Prop] -> Prop -> Prop -> Rule Prop
arrE gamma delta a b =
    [gamma :|-: a :=>: b,
     delta :|-: a]
    :---:
    gamma ++ delta :|-: b

andI :: [Prop] -> Prop -> [Prop] -> Prop -> Rule Prop
andI gamma a delta b =
    [gamma :|-: a,
     delta :|-: b]
    :---:
    gamma ++ delta :|-: a :*: b

andE :: [Prop] -> [Prop] -> Prop -> Prop -> Prop -> Rule Prop
andE gamma delta a b c =
    [gamma :|-: a :*: b,
     delta ++ [a, b] :|-: c]
    :---:
    gamma ++ delta :|-: c

orI1 :: [Prop] -> Prop -> Prop -> Rule Prop
orI1 gamma a b = 
    [gamma :|-: a]
    :---:
    gamma :|-: a :+: b

orI2 :: [Prop] -> Prop -> Prop -> Rule Prop
orI2 gamma a b =
    [gamma :|-: b]
    :---:
    gamma :|-: a :+: b

orE :: [Prop] -> [Prop] -> Prop -> Prop -> Prop -> Rule Prop
orE gamma delta a b c =
    [gamma :|-: a :+: b,
     delta ++ [a] :|-: c,
     delta ++ [b] :|-: c]
    :---:
    gamma ++ delta :|-: c

instance Subst Prop where
    subs from to p =
        if p == from
        then to
        else case p of
          Const _ -> p
          p1 :*: p2 -> (subs from to p1) :*: (subs p2 from to)
          p1 :+: p2  -> (subs from to p1) :+: (subs from to p2)
          p1 :=>: p2  -> (subs from to p1) :=>: (subs from to p2)

instance Subst (Jmt Prop) where
    subs from to (e :|-: p) = (map (subs from to) e) :|-: (subs from to p)

instance Show (Rule Prop) where
    show (prems :---: conc) = concat (map ((++"\n") . show) prems) ++ "-------------------\n" ++ show conc

instance Show (Jmt Prop) where
    show (env :|-: prop) = show env ++ " |- " ++ show prop

instance Show Prop where
    show (Const str) = str
    show (p1 :*: p2) = "(" ++ show p1 ++ " * " ++ show p2 ++ ")"
    show (p1 :+: p2) = "(" ++ show p1 ++ " + " ++ show p2 ++ ")"
    show (p1 :=>: p2) = "(" ++ show p1 ++ " => " ++ show p2 ++ ")"

class Subst a where
    subs :: Prop -> Prop -> a -> a

subsEnv :: [Prop] -> [Prop] -> Rule Prop -> Rule Prop
subsEnv from to (precs :---: conc_env :|-: conc_prop) =
    map (\(env :|-: prop) -> subsInSingleEnv from to env :|-: prop) precs
    :---:
    subsInSingleEnv from to conc_env :|-: conc_prop
    where
      subsInSingleEnv :: [Prop] -> [Prop] -> [Prop] -> [Prop]
      subsInSingleEnv from to env =
          if from `elem` (subsequences env)
          then (env \\ from) ++ to
          else env
