module Type
  ( Type(..)
  , TVar(..)
  , Subst
  , suEmpty
  , suExtend
  , suUnchecked
  , ApSusbt(..)
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)

import Syntax(Name)

data Type = TVar TVar
          | TCon Name [Type]
          | TNot Type
            deriving Show

data TVar = TV !Int (Maybe Name)
            deriving Show

instance Eq TVar where
  TV x _ == TV y _ = x == y

instance Ord TVar where
  compare (TV x _) (TV y _) = compare x y



--------------------------------------------------------------------------------
newtype Subst = Subst (Map TVar Type)

suEmpty :: Subst
suEmpty = Subst Map.empty

suUnchecked :: [(TVar,Type)] -> Subst
suUnchecked xs = Subst (Map.fromList xs)

suExtend :: TVar -> Type -> Subst -> Maybe Subst
suExtend x (TVar y) su | x == y     = Just su
suExtend x t0 _ | occurs t0 = Nothing
  where occurs ty = case ty of
                      TVar y    -> x == y
                      TCon _ ts -> any occurs ts
                      TNot t    -> occurs t 
suExtend x t su = Just (Subst (Map.insert x t mp1))
  where Subst mp1 = apSubst (Subst (Map.singleton x t)) su

suCompose :: Subst -> Subst -> Subst
suCompose su2@(Subst mp2) su1 = Subst (Map.union mp1 mp2)
  where Subst mp1 = apSubst su2 su1


suLookup :: TVar -> Subst -> Maybe Type
suLookup x (Subst mp) = Map.lookup x mp


class ApSusbt t where
  apSubst :: Subst -> t -> t

instance ApSusbt a => ApSusbt [a] where
  apSubst su xs = apSubst su <$> xs

instance ApSusbt Subst where
  apSubst su (Subst mp) = Subst (apSubst su <$> mp)

instance ApSusbt Type where
  apSubst su ty =
    case ty of
      TVar x    -> fromMaybe ty (suLookup x su)
      TCon c ts -> TCon c (apSubst su ts)
      TNot t    -> case apSubst su t of
                     TNot t' -> t'
                     t'      -> TNot t'






