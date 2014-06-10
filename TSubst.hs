module TSubst where

import Data.List
import Type

type TSubst = [(TVar,Type)]
    
-- Utilities

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

--

inTS :: TVar -> Type -> Bool
inTS v (AtomType v')   = v==v'
inTS v (FunType t1 t2) = (inTS v t1) || (inTS v t2)
    
replaceTS :: TVar -> Type -> TSubst -> TSubst
replaceTS v t ts =
    map (mapSnd (replaceVar v t)) ts
    where replaceVar v t t' =
            case t' of
                AtomType v'   -> if v==v'
                                    then t
                                    else t'
                FunType t1 t2 -> FunType (replaceVar v t t1) (replaceVar v t t2)
    
concatTSubst :: TSubst -> TSubst -> TSubst
concatTSubst = foldl (flip $ uncurry appendTS)

-- PRE: t1!=t2
appendTS :: TVar -> Type -> TSubst -> TSubst
appendTS v t2 ts = 
    if v `inTS` t2
        then error ("Occurs check: No se puede hacer la substitución "++
                    (showTVar v)++" = "++(show t2))
        else
            case lookup v ts of
                Nothing -> (v,t2):(replaceTS v t2 ts)
                Just t  -> let (t',ts') = unify t t2 in
                             appendTS v t' (concatTSubst (elim v ts) ts')
    
    where elim v ts = filter (\(a,b) -> a/=v) ts
        
-- Dados dos tipos, devuelve un tipo y la substitución necesaria
-- para igualarlos
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

-- Aplica una substitución a un tipo                    
apply :: TSubst -> Type -> Type
apply [] t = t
apply tsust@((v,t'):ts) t'' =
    case t'' of
        AtomType v' -> if v==v' 
                        then apply ts t'
                        else apply ts t''
        FunType t1 t2 -> FunType (apply tsust t1) (apply tsust t2)
            
-- Dado un tipo cambia las variables de tipo por otras con menores índices
reduceIndexes :: Type -> Type
reduceIndexes t = apply (map (mapSnd AtomType) (getMinVars (allTVars t)))
                        t
                        