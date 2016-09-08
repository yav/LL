module Check where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad(unless,zipWithM_,liftM,ap,forM)

import Syntax hiding (Type(..))
import qualified Syntax as S
import Type


newtype Env = Env (Map Name Type)


inferExpr :: Expr -> InferM Env
inferExpr expr =

  case expr of

    EId x' y' ->
      do (x,tx) <- inferTypedName x'
         (y,ty) <- inferTypedName y'
         fits tx ty
         listEnv [ (x, tx), (y, ty) ]

    ECon x' c xs ->
      do (t,cs) <- lookupDataByCon c

         ts <- case lookup c cs of
                 Nothing -> reportError "[bug] Missing constructor."
                 Just ss -> return ss

         xts <- mapM inferTypedName xs
         exactZipWithM fits ts (map snd xts)

         (x,t') <- inferTypedName x'
         unify t t'

         listEnv ((x,t) : xts)

    ECase0 x' d xs ->
      do (t,cs) <- lookupData d
         unless (null cs) (reportError "Empty case on a non-empty datatype")
         xts <- mapM inferTypedName xs
         (x,t') <- inferTypedName x'
         fits t t'
         listEnv ((x,t') : xts)


    ECase _ [] -> reportError "[bug] Emptty case."

    ECase x' as@((PCon c0 _,_) : _) ->
      do (t,cs)     <- lookupDataByCon c0
         env : envs <- mapM (check cs) as
         let cons    = [ c | (PCon c _, _) <- as ]
             missing = [ c | c <- cons, not (c `elem` map fst cs) ]

         distinct cons
         unless (null missing) $ reportError "Missing cases"

         mapM_ (sameEnv env) envs
         (x,t') <- inferTypedName x'
         fits t t'
         extendEnv x t' env

      where
      check cs (PCon c xs, e) =
        case lookup c cs of
          Nothing -> reportError "Datatype has no such constructor"
          Just ts ->
            do xts <- mapM inferTypedName xs
               distinct (map name xs)
               exactZipWithM fits ts (map snd xts)
               checkUsed xts =<< inferExpr e

    ECut x' e1 y' e2 ->
      do xt <- inferTypedName x'
         yt <- inferTypedName y'
         fits (snd xt) (snd yt)

         env1  <- checkUsed [xt] =<< inferExpr e1
         env2  <- checkUsed [yt] =<< inferExpr e2
         mergeEnv env1 env2

    ENamed x -> lookupNamed x


inferTypedName :: TypedName -> InferM (Name, Type)
inferTypedName tn =
  case signature tn of
    Nothing   -> do t <- newTVar
                    return (name tn, t)
    Just sig  -> do t <- checkType sig
                    return (name tn, t)


-- XXX: Does not check for duplicates
checkDecl :: Decl -> InferM Data
checkDecl (DData c xs cons) =
  do setKnownLimit xs
     ts <- mapM checkTVar xs
     let vs = [ x | TVar x <- ts ]
     cs <- mapM checkDCon cons
     return (CheckedData c vs cs)

checkDCon :: DataCon -> InferM (Name,[Type])
checkDCon (DataCon c ts') =
  do ts <- mapM checkType ts'
     return (c,ts)


checkType :: S.Type -> InferM Type
checkType ty =
  case ty of
    S.TNot t' ->
      do t <- checkType t'
         case t of
           TNot a -> return a
           _      -> return (TNot t)

    S.TCon c ts' ->
      do ts <- mapM checkType ts'
         (ty',_) <- lookupData c
         unify ty' (TCon c ts)
         return ty'

    S.TVar x ->
      do mb <- IM $ \_ rw -> Right (Map.lookup x (knownVars rw), rw)
         case mb of
           Nothing ->
             do t <- newTVar
                IM $ \_ rw -> Right (t, rw { knownVars =
                                              Map.insert x t (knownVars rw) })
           Just t -> return t

checkTVar :: Name -> InferM Type
checkTVar x =
  do mb <- lookupKnown x
     case mb of
       Just t -> return t
       Nothing ->
         do flex <- flexibleKnown x
            unless flex $ reportError "Unknown parameter"
            t <- newTVar
            addKnown x t
            return t


--------------------------------------------------------------------------------
newtype InferM a = IM (RO -> RW -> Either TypeError (a,RW))

type TypeError = String

data RW = RW
  { nextVar   :: !Int
  , subst     :: !Subst
  , knownVars :: !(Map Name Type)
  , limitVars :: Maybe [Name]
  }

data Data   = CheckedData Name [TVar] [ (Name, [Type]) ]
data Schema = Forall [TVar] Env

data RO = RO
  { dataDecls       :: Map Name Data
  , dataDeclsByCon  :: Map Name Data
  , namedSchemas    :: Map Name Schema
  }


instance Functor InferM where
  fmap = liftM

instance Applicative InferM where
  pure a = IM (\_ rw -> Right (a,rw))
  (<*>)  = ap

instance Monad InferM where
  IM m >>= f = IM (\ro rw -> case m ro rw of
                               Left err -> Left err
                               Right (a,rw1) -> let IM m1 = f a
                                             in m1 ro rw1)

lookupKnown :: Name -> InferM (Maybe Type)
lookupKnown x = IM $ \_ rw -> Right (Map.lookup x (knownVars rw), rw)

addKnown :: Name -> Type -> InferM ()
addKnown x t = IM $ \_ rw ->
  Right ((), rw { knownVars = Map.insert x t (knownVars rw) })

setNoKnownLimit :: InferM ()
setNoKnownLimit = IM $ \_ rw -> Right ((), rw { limitVars = Nothing
                                              , knownVars = Map.empty })

setKnownLimit :: [Name] -> InferM ()
setKnownLimit xs = IM $ \_ rw -> Right ((), rw { limitVars = Just xs
                                               , knownVars = Map.empty })

flexibleKnown :: Name -> InferM Bool
flexibleKnown x = IM $ \_ rw ->
  case limitVars rw of
    Nothing -> Right (True, rw)
    Just xs -> Right (x `elem` xs, rw)


reportError :: TypeError -> InferM a
reportError err = IM (\_ _ -> Left err)

newTVar :: InferM Type
newTVar = IM $ \_ rw ->
  let n  = nextVar rw
      n1 = n + 1
      x  = TV n Nothing
  in x `seq` n1 `seq` Right (TVar x, rw { nextVar = n + 1 })

unify :: Type -> Type -> InferM ()
unify t1' t2' =
  do t1 <- zonk t1'
     t2 <- zonk t2'
     case (t1,t2) of
       (TNot s, _) -> fits s t2
       (_, TNot s) -> fits t1 s
       (TVar x, _) -> bindVar x t2
       (_, TVar x) -> bindVar x t1
       (TCon c1 ts1, TCon c2 ts2)
         | c1 == c2 -> exactZipWithM unify ts1 ts2
       _ -> reportError "Type mismatch."

fits :: Type -> Type -> InferM ()
fits t1' t2' =
  do t1 <- zonk t1'
     t2 <- zonk t2'
     case (t1,t2) of
       (TNot x, _) -> unify x t2
       (_, TNot x) -> unify t1 x
       (TVar x, _) -> bindVar x (TNot t2)
       (_, TVar x) -> bindVar x (TNot t1)
       _           -> reportError "Types do not fit."

exactZipWithM :: (Type -> Type -> InferM ()) ->
                 [Type] -> [Type] -> InferM ()

exactZipWithM f (x : xs) (y : ys) = f x y >> exactZipWithM f xs ys
exactZipWithM _ [] []             = return ()
exactZipWithM _ _ _               = reportError "Arity mismatch."

bindVar :: TVar -> Type -> InferM ()
bindVar x t = IM $ \_ rw ->
  case suExtend x t (subst rw) of
    Nothing  -> Left "Recursive type."
    Just su1 -> Right ((), rw { subst = su1 })


listEnv :: [(Name,Type)] -> InferM Env
listEnv bs =
  do distinct (map fst bs)
     return (Env (Map.fromList bs))

extendEnv :: Name -> Type -> Env -> InferM Env
extendEnv x t (Env mp)
  | x `Map.member` mp = reportError "Multiple uses."
  | otherwise         = return (Env (Map.insert x t mp))

mergeEnv :: Env -> Env -> InferM Env
mergeEnv (Env mp1) (Env mp2)
  | null bad  = return (Env (Map.union mp1 mp2))
  | otherwise = reportError "Name classh"
  where
  bad = Map.keys (Map.intersection mp1 mp2)

sameEnv :: Env -> Env -> InferM ()
sameEnv (Env xs) (Env ys) =
  do unless (Map.keys xs == Map.keys ys)
       $ reportError "Diffirent names in cases"
     zipWithM_ unify (Map.elems xs) (Map.elems ys)



distinct :: [Name] -> InferM ()
distinct [] = return ()
distinct (x : xs) | x `elem` xs = reportError ("Multiple " ++ show x)
distinct (_ : xs) = distinct xs


checkUsed :: [(Name,Type)] -> Env -> InferM Env
checkUsed [] env = return env
checkUsed ((x,t):xs) (Env mp) =
  case Map.lookup x mp of
    Just t1 -> do unify t t1
                  checkUsed xs (Env (Map.delete x mp))
    Nothing -> reportError "Unused variable."

lookupNamed :: Name -> InferM Env
lookupNamed x =
  do mb <- IM $ \ro rw -> Right (Map.lookup x (namedSchemas ro), rw)
     case mb of
       Nothing -> reportError "Unknwon named expression"
       Just (Forall xs (Env mp)) ->
         do bs <- forM xs $ \v ->
                    do t <- newTVar
                       return (v,t)
            let su = suUnchecked bs
            return (Env (fmap (apSubst su) mp))

type DCon = (Name, [Type])

lookupData :: Name -> InferM (Type, [DCon])
lookupData d =
  do mb <- IM $ \ro rw -> Right (Map.lookup d (dataDecls ro), rw)
     case mb of
        Nothing -> reportError "Unknown datatype"
        Just da -> freshData da

lookupDataByCon :: Name -> InferM (Type, [DCon])
lookupDataByCon c =
  do mb <- IM $ \ro rw -> Right (Map.lookup c (dataDeclsByCon ro), rw)
     case mb of
       Nothing -> reportError "Unknwon constructor"
       Just d  -> freshData d

freshData :: Data -> InferM (Type, [DCon])
freshData (CheckedData x vs cs) =
  do bs <- forM vs $ \v -> do t <- newTVar
                              return (v,t)
     let su = suUnchecked bs
         ty = TCon x (map snd bs)
         cons = [ (c, apSubst su ts) | (c,ts) <- cs ]

     return (ty, cons)


zonk :: Type -> InferM Type
zonk t = IM $ \_ rw -> Right (apSubst (subst rw) t, rw)



