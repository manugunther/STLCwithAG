module TSubst where

import Data.List
import Type


-- Type for substitution of type variables
type TSubst = [(TVar,Type)]
    

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

--


inType :: TVar -> Type -> Bool
inType v (AtomType v')   = v==v'
inType v (FunType t1 t2) = (inType v t1) || (inType v t2)
    
    
-- Replace v by t in substitution ts
replaceTS :: TVar -> Type -> TSubst -> TSubst
replaceTS v t ts =
    map (mapSnd (repVar v t)) ts
    where repVar v t t' =
            case t' of
                AtomType v'   -> if v==v'
                                    then t
                                    else t'
                FunType t1 t2 -> FunType (repVar v t t1) (repVar v t t2)
    


concatTS :: TSubst -> TSubst -> TSubst
concatTS ts []          = ts
concatTS ts ((v,t):ts') = concatTS (addTS v t ts) ts'

-- Add a pair (v,t) to substitution and performs unification if
-- necessary.
addTS :: TVar -> Type -> TSubst -> TSubst
addTS v t2 ts = 
    if v `inType` t2
        then error ("Occurs check: Cannot construct the type "++
                    (showTVar v)++" = "++(show t2))
        else
            case lookup v ts of
                Nothing -> (replaceTS v t2 ts)++[(v,t2)]
                Just t  -> let (t',ts') = unify t t2 in
                             concatTS (addTS v t' (elim v ts)) ts'
    
    where elim v ts = filter (\(a,b) -> a/=v) ts
        
-- Unify two types, return one of these and the substitution performed
unify :: Type -> Type -> (Type,TSubst)
unify t1 t2 =
    case t1 of
        AtomType v   -> case t2 of
                            AtomType v'  -> if v==v'
                                                then (t2,[])
                                                else (t2,[(v,t2)])
                            _            -> (t2,[(v,t2)])
        FunType t t' -> case t2 of
                            AtomType v   -> (t1,[(v,t1)])
                            FunType s s' -> (t2,(snd $ unify t s)++
                                            (snd $ unify t' s'))

-- Applies a substitution to a type
apply :: TSubst -> Type -> Type
apply [] t = t
apply tsust@((v,t'):ts) t'' =
    case t'' of
        AtomType v' -> if v==v' 
                        then apply ts t'
                        else apply ts t''
        FunType t1 t2 -> FunType (apply tsust t1) (apply tsust t2)
            
-- Replace variables in a type by others with lower indices
reduceIndexes :: Type -> Type
reduceIndexes t = apply (map (mapSnd AtomType) (getMinVars (allTVars t)))
                        t
                        