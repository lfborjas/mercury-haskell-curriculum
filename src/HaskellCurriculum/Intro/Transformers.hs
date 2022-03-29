{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE FlexibleInstances #-}
-- | <https://github.com/MercuryTechnologies/haskell-curriculum/tree/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/intro/transformers>
module HaskellCurriculum.Intro.Transformers where

import Data.Functor.Identity

-- | Manually defined reader:

newtype Reader r a = Reader { runReader :: r -> a}

-- | Monad transformer version:
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f a = ReaderT $ fmap f . runReaderT a

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . const . pure
  mf <*> ma = ReaderT $ \r -> runReaderT mf r <*> runReaderT ma r

instance Monad m => Monad (ReaderT r m) where
  ma >>= k = ReaderT $ \r -> do
    a <- runReaderT ma r
    runReaderT (k a) r

-- EXERCISES:

-- write reader in terms of ReaderT, implement runReader
type Reader' r a = ReaderT r Identity a
runReader' :: Reader' r a -> r -> a
runReader' r ctx = runIdentity $ runReaderT r ctx

-- implement StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f a =
    StateT $ fmap both . runStateT a
    where
      both (ma, s') = (f ma, s')

-- NOTE(luis) interesting that https://hackage.haskell.org/package/transformers-0.6.0.4/docs/src/Control.Monad.Trans.State.Lazy.html#line-206
-- uses a @Monad@ constraint here, to make the implementation of @<*>@ easier.
-- It is actually /impossible/ to write it without the Monad constraint: there's an inevitable
-- binding that needs to happen between the state produced by mf and to be used by ma
instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  StateT mf <*> StateT ma =
    StateT withNewState
    where
      withNewState s = do
        (f, s') <- mf s
        (a, s'') <- ma s'
        pure (f a, s'')

instance Monad m => Monad (StateT s m) where
  ma >>= k = StateT $ \s -> do
    (a, s') <- runStateT ma s
    runStateT (k a) s'

--- section 2:
-- https://github.com/MercuryTechnologies/haskell-curriculum/tree/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/intro/transformers#what-does-this-mean

-- | Non-typeclass version of @ask@
askR :: Applicative m => ReaderT r m r
askR = ReaderT pure

liftRT :: m a -> ReaderT r m a
-- NOTE(luis) the doc says @ReaderT $ \r -> action@
-- in both cases it's apparent that we're "ignoring" the
-- reader context and simply returning @action@ within ReaderT
-- "when we're lifting, we can't modify the type we're lifting into"
liftRT action = ReaderT $ const action

-- >>> runReaderT printEnv True
printEnv :: Show r => ReaderT r IO ()
printEnv = do
  r <- ask
  liftRT $ print r


-- Generalizing lift:

class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

-- Exercises

-- implement lift for -- StateT
instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    pure (a, s)


getS :: Applicative m => StateT s m s
getS = StateT $ \s -> pure (s, s)

-- NOTE(luis) interesting that in the transformers package,
-- this is what they do:
--https://hackage.haskell.org/package/transformers-0.6.0.4/docs/src/Control.Monad.Trans.State.Lazy.html#state
get'  :: Monad m => StateT s m s
get' =
  state $ \s -> (s, s)

state :: Monad m => (s -> (a,s)) -> StateT s m a
state f = StateT (pure . f)

-- implement a printState for StateT
printState :: Show s => StateT s IO ()
printState = do
  s <- getS
  liftIO $ print s


-- LIFTING HIGHER
-- https://github.com/MercuryTechnologies/haskell-curriculum/tree/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/intro/transformers#lifting-higher

-- NOTE(luis) not in the doc:
-- https://hackage.haskell.org/package/transformers-0.6.0.4/docs/src/Control.Monad.Trans.State.Lazy.html#evalState
evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
  (a, _s') <- runStateT m s
  pure a

-- Old definition:
-- newtype AppT r s a =
--   AppT { unAppT :: ReaderT r (StateT s IO) a }
--   deriving newtype (Functor, Applicative, Monad)

-- runAppT :: r -> s -> AppT r s a -> IO a
-- runAppT r s app = evalStateT (runReaderT (unAppT app) r) s

-- -- | Ugly function that runs into the "lifting a lot" issue
-- printAppEnv :: Show r => AppT r s ()
-- printAppEnv = do
--   env <- AppT askR
--   AppT . lift . lift . print $ env

-- to solve, we can add a new typeclass to generalize @ask@:

-- NOTE(luis) the functional dependency helps us dictate
-- that the monad will determine the reader context --
-- in the case of the ReaderT instance, the context of the
-- ReaderT monad is the same as the MonadReader context
class Monad m => MonadReader r m | m -> r where
  ask :: m r

-- TODO(luis) this requires @FlexibleInstances@, and it is not mentioned in the doc
instance Monad m => MonadReader r (ReaderT r m) where
  ask = ReaderT pure

-- newtype AppT r s a =
--   AppT { unAppT :: ReaderT r (StateT s IO) a }
--   deriving newtype (MonadReader r, Functor, Applicative, Monad)

-- runAppT :: r -> s -> AppT r s a -> IO a
-- runAppT r s app = evalStateT (runReaderT (unAppT app) r) s

-- -- | Ugly function that runs into the "lifting a lot" issue
-- printAppEnv :: Show r => AppT r s ()
-- printAppEnv = do
--   -- NOTE(luis) without functional dependencies,
--   -- this will error out with the could not deduce:
--   env <- ask
--   AppT . lift . lift . print $ env

-- Exercises

-- implement the MonadState typeclass

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m () -- NOTE(luis) makes sense that we void-ify the result: we only care about the state

instance Monad m => MonadState s (StateT s m) where
  get = get'
  put s = state $ const ((), s)


-- GENERALIZING FOR IO
-- https://github.com/MercuryTechnologies/haskell-curriculum/tree/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/intro/transformers#generalizing-a-solution-for-io

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

newtype AppT r s a =
  AppT { unAppT :: ReaderT r (StateT s IO) a }
  deriving newtype (MonadIO, MonadReader r, Functor, Applicative, Monad)

runAppT :: r -> s -> AppT r s a -> IO a
runAppT r s app = evalStateT (runReaderT (unAppT app) r) s

-- | Ugly function that runs into the "lifting a lot" issue
printAppEnv :: Show r => AppT r s ()
printAppEnv = do
  -- NOTE(luis) without functional dependencies,
  -- this will error out with the could not deduce:
  env <- ask
  --liftIO . print $ env
  printLifted env

-- BONUS: lifted print:

printLifted :: (MonadIO m, Show a) => a -> m ()
printLifted = liftIO . print
