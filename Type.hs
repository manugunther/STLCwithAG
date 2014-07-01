module Type where

import Data.List
import UU.Pretty
import qualified Data.Set as S

-- Syntax for types:
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

-- Returns all variables occurring on a type.
allTVars :: Type -> S.Set TVar
allTVars (AtomType v)    = S.singleton v
allTVars (FunType t1 t2) = (allTVars t1) `S.union` (allTVars t2)
    
-- Returns a fresh variable
freshTVar :: TVar -> TVar
freshTVar = nextTVar

nextTVar :: TVar -> TVar
nextTVar = (+1)
    
-- From a set of variables returns a substitution of
-- variables with lower indices.
getMinVars :: S.Set TVar -> [(TVar,TVar)]
getMinVars s = zip l [0..length l]
    where l = S.toAscList s

