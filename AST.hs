import qualified Data.Map as Map
import           Data.Map (Map)
import           Control.Monad(unless, when, replicateM, forM, foldM)

data Type = TCon TCon [Type]
          | TVar TVar

data TVar = TV !Int (Maybe String)
            deriving (Eq,Ord)

data TCon = TC_with | TC_par | TC_plus | TC_times

          | TC_fits
            deriving Eq

newtype Name = N String
               deriving (Eq,Ord)

data Expr      = E_id     Name Name
               | E_plus   Name Name Int Int  -- position, total number
               | E_times  Name [Name]
               | E_with   Name [(Name,Expr)] -- non-empty
               | E_with0  Name [Name]
               | E_par    Name [Name] Expr

               | E_cut Name Expr Name Expr

tFits :: Type -> Type -> Type
tFits a b = TCon TC_fits [a,b]

tPlus :: [Type] -> Type
tPlus ts = TCon TC_plus ts

tTimes :: [Type] -> Type
tTimes ts = TCon TC_times ts

tWith :: [Type] -> Type
tWith ts = TCon TC_with ts

tPar :: [Type] -> Type
tPar ts = TCon TC_par ts


--------------------------------------------------------------------------------

newtype Env   = Env (Map Name Type)
newtype Subst = Subst (Map TVar Type)

infer :: Expr -> InferM Env
infer expr =
  case expr of

    E_id x y ->
      do t1 <- newTVar Nothing
         t2 <- newTVar Nothing
         fit t1 t2
         listEnv [(x,t1), (y,t2)]

    E_cut x e1 y e2 ->
      do (t1,env1) <- lkp x =<< infer e1
         (t2,env2) <- lkp y =<< infer e2
         fit t1 t2
         mergeEnv env1 env2

    E_plus x y pos tot ->
      do unless (0 <= pos && pos < tot)
          $ reportError "Malformed E_plus"
         t : ts <- replicateM (1 + tot) (newTVar Nothing)
         let s = ts !! pos
         fit t s
         listEnv [(x, tPlus ts), (y, t)]

    E_times x xs ->
      do (vs,ts) <- fmap unzip $ forM xs $ \v -> do s <- newTVar Nothing
                                                    t <- newTVar Nothing
                                                    fit s t
                                                    return ((v,s), t)
         listEnv ((x, tTimes ts) : vs)

    E_with0 x xs ->
      do vs  <- forM xs $ \v -> do t <- newTVar Nothing
                                   return (v,t)
         listEnv ((x, tWith []) : vs)

    E_with x alts ->
      do when (null alts) $ reportError "Malformed W_with"
         (ts,env:envs) <- fmap unzip $ forM alts $ \(v,e) -> lkp v =<< infer e
         mapM_ (sameEnv env) envs
         extendEnv x (tWith ts) env

    E_par x xs e ->
      do env1     <- infer e
         let step (ts,env) v = do (t,env') <- lkp v env
                                  return (t:ts,env')
         (ts,env) <- foldM step ([], env1) xs
         extendEnv x (tPar (reverse ts)) env





--------------------------------------------------------------------------------

type InferM = IO
type TypeError = String

reportError :: TypeError -> InferM a
reportError = undefined

newTVar :: Maybe String -> InferM Type
newTVar = undefined

constrain :: Type -> InferM ()
constrain = undefined

listEnv :: [(Name,Type)] -> InferM Env
listEnv = undefined

extendEnv :: Name -> Type -> Env -> InferM Env
extendEnv = undefined

mergeEnv :: Env -> Env -> InferM Env
mergeEnv = undefined

sameEnv :: Env -> Env -> InferM ()
sameEnv = undefined

lkp :: Name -> Env -> InferM (Type, Env)
lkp = undefined

fit :: Type -> Type -> InferM ()
fit x y = constrain (tFits x y)


