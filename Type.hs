module Type where

import Data.List
import UU.Pretty
import qualified Data.Set as S

data Type = AtomType TVar | FunType Type Type
    deriving Eq

instance Show Type where
    show (AtomType v) = showTVar v
    show (FunType t1 t2) = (parents t1) ++" -> "++(show t2)
    
parents :: Type -> String
parents t@(FunType t1 t2) = "("++(show t)++")"
parents t                 = show t
    
type TVar = Int

stringTVar = "t"

showTVar :: TVar -> String
showTVar 0 = stringTVar
showTVar n = stringTVar ++ (show n)


initTVar :: TVar
initTVar = 0

-- Dado una lista de tipos, devuelve todas las TVar que ocurren
-- en los tipos, sin repeticiones
allTVars :: Type -> S.Set TVar
allTVars (AtomType v)    = S.singleton v
allTVars (FunType t1 t2) = (allTVars t1) `S.union` (allTVars t2)
    
-- Genera una variable de tipo fresca para una lista de variables
freshTVar :: S.Set TVar -> TVar
freshTVar = freshTVar' 0

freshTVar' :: Int -> S.Set TVar -> TVar
freshTVar' i vs = if S.member i vs 
                        then freshTVar' (i+1) vs
                        else snew
    where snew = i


-- Dado un conjunto de variables, devuelve
-- una substitución de variables por variables,
-- donde las resultantes tendrán los menores coeficientes
-- posibles
getMinVars :: S.Set TVar -> [(TVar,TVar)]
getMinVars s = zip l (ln (length l))
    where l = S.toAscList s
          ln n = [0..n]

