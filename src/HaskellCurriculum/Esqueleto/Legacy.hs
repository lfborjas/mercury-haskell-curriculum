{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- |From: https://github.com/MercuryTechnologies/esqueleto-training/blob/master/src/Legacy.hs

module HaskellCurriculum.Esqueleto.Legacy where

import Control.Monad
import Data.Coerce
import Data.Map (Map)
import Data.Maybe hiding (isNothing)
import Database.Esqueleto.Legacy
import Database.Esqueleto.Internal.Internal

import HaskellCurriculum.Esqueleto.Models
import Database.Esqueleto.Experimental (case_)
import Database.Esqueleto.Experimental.ToMaybe (toMaybe)

-- | Replicating persistent's 'get' in Esqueleto
get :: (PersistRecordBackend entity SqlBackend) => Key entity -> SqlPersistT IO (Maybe (Entity entity))
get k = do
  result <-
    -- NOTE: select will transform a 'SqlQuery' into a list of results
    --  select :: forall a r (m :: * -> *).
    -- (SqlSelect a r, MonadIO m) =>
    -- SqlQuery a -> SqlReadT m [r]
    -- which Matt's notes simplify/equate to:
    -- select :: (SqlSelect a r, MonadIO m, BackendCompatible SqlBackend backend, PersistQueryRead backend, PersistUniqueRead backend)
    --    => SqlQuery a -> ReaderT backend m [r]
    -- select :: (SqlSelect a r)
    --    => SqlQuery a -> SqlPersistT IO [r]
    -- @SqlQuery@ has a monad instance, so we can "accrue"/compose queries with bind/do notation
    --
    select $
    -- Of note in 'from', is the @From@ typeclass: Entity,tuples and 'JoinKind' are instances of it
    -- abridged: from :: From a => (a -> SqlQuery b) -> SqlQuery b
    from $ \table -> do
    -- where_ :: SqlExpr (Value Bool) -> SqlQuery ()
    -- 'Value' being a newtype indicating a single column type
    -- 'table' is a SqlExpr that refers to the table as is known to the backend
    -- 'persistIdField' is @persistIdField :: (PersistEntity rec) => EntityField rec (Key rec)@, i.e., a generic way
    --   to get the key from an entity
    -- note that the '==.' operator /here/ is Esqueleto's, not Persistent's, so one has to import qualified.
    -- 'val' will construct a value out of a haskell value: @val :: PersistField typ => tpe -> SqlExpr (Value typ)@
    -- since we can't use Haskell values directly
    where_ $ table ^. persistIdField ==. val k
    limit 1
    -- what we return can be anything that we could send to 'from': an Entity, a tuple, a JoinKind, after all,
    -- we're going back into the 'select' monadic operation, which was wrapping a SqlExpr (Entity entity) in a SqlQuery
    pure table
  pure $
    case result of
      [x] -> Just x
      _ -> Nothing

-- | Writing joins:
-- We can join entities with 'JoinKind's; note that in newer versions of Esqueleto,
-- it doesn't matter in which order we introduce the @on@ clauses.
usersWithOrganizations1 :: SqlPersistT IO [(Entity User, Entity Organization)]
usersWithOrganizations1 =
  select $
  -- InnerJoin :: SqlExpr -> SqlExpr -> SqlExpr
  -- i.e. we're building sql exprs literally joining the component exprs referencing all three tables
  from $ \(user `InnerJoin` userOrg `InnerJoin` organization) -> do
  on $ user ^. UserId ==. userOrg ^. UserOrganizationUser
  on $ userOrg ^. UserOrganizationOrganization ==. organization ^. OrganizationId
  pure (user, organization)

-- | Doing left outer joins to also reflect users that don't have organizations:
-- note that nothing in persistent prevents us from doing this, which I believe would
-- be a runtime error: rows with a null org (the whole point of outer join) won't
-- correctly parse as the expected organization entity
usersWithOrganizations2 :: SqlPersistT IO [(Entity User, Entity Organization)]
usersWithOrganizations2 =
  select $
  -- InnerJoin :: SqlExpr -> SqlExpr -> SqlExpr
  -- i.e. we're building sql exprs literally joining the component exprs referencing all three tables
  from $ \(user `LeftOuterJoin` userOrg `LeftOuterJoin` organization) -> do
  on $ user ^. UserId ==. userOrg ^. UserOrganizationUser
  on $ userOrg ^. UserOrganizationOrganization ==. organization ^. OrganizationId
  pure (user, organization)

-- | To /actually/ have a working query, we need to project with '?.' and 'just' as a counterpart
-- to operate on optional values on both sides
usersWithOrganizations :: SqlPersistT IO [(Entity User, Maybe (Entity Organization))]
usersWithOrganizations =
  select $
  from $ \(user `LeftOuterJoin` userOrg `LeftOuterJoin` organization) -> do
  -- NOTE: due to the way we're joining, both the join table and the org need to project
  -- optional fields
  -- @just@ takes a sql expr representing a value and returns a maybe expr for that value
  --   i.e. just :: SqlExpr (Value typ) -> SqlExpr (Value (Maybe typ))
  -- @?.@ projects a maybe value out of a maybe entity:
  --   i.e. (?.):: SqlExpr (Maybe (Entity val)) -> EntityField val typ -> SqlExpr (Value (Maybe typ))
  on $ just (user ^. UserId) ==. userOrg ?. UserOrganizationUser
  on $ userOrg ?. UserOrganizationOrganization ==. organization ?. OrganizationId
  pure (user, organization)

-- | We can also take the very repetitive, raw-row representation into something
-- more associative:
-- associateJoin :: forall e1 e0.
-- Ord (Key e0) =>
-- [(Entity e0, e1)] -> Map (Key e0) (e0, [e1])
-- some more docs: https://hackage.haskell.org/package/esqueleto-3.5.4.0/docs/Database-Esqueleto.html#v:associateJoin
-- and the source, which simply does a clever foldr + insertWith:
-- @since 3.1.2
-- associateJoin
--     :: forall e1 e0.  Ord (Key e0)
--     => [(Entity e0, e1)]
--     -> Map.Map (Key e0) (e0, [e1])
-- associateJoin = foldr f start
--   where
--     start = Map.empty
--     f (one, many) =
--         Map.insertWith
--             (\(oneOld, manyOld) (_, manyNew) -> (oneOld, manyNew ++ manyOld ))
--             (entityKey one)
--             (entityVal one, [many])
usersWithOrganizationsMap :: SqlPersistT IO (Map UserId (User, [Maybe (Entity Organization)]))
usersWithOrganizationsMap = associateJoin <$> usersWithOrganizations

-- | The composability of Esqueleto is nice: we can re-use @SqlQuery@ values by simply
-- not @select@ing:
usersWithOrganizationsQuery :: SqlQuery
  (SqlExpr (Entity User), SqlExpr (Maybe (Entity Organization)))
usersWithOrganizationsQuery =
  from $ \(user `LeftOuterJoin` userOrg `LeftOuterJoin` organization) -> do
  -- NOTE: due to the way we're joining, both the join table and the org need to project
  -- optional fields
  -- @just@ takes a sql expr representing a value and returns a maybe expr for that value
  --   i.e. just :: SqlExpr (Value typ) -> SqlExpr (Value (Maybe typ))
  -- @?.@ projects a maybe value out of a maybe entity:
  --   i.e. (?.):: SqlExpr (Maybe (Entity val)) -> EntityField val typ -> SqlExpr (Value (Maybe typ))
  on $ just (user ^. UserId) ==. userOrg ?. UserOrganizationUser
  on $ userOrg ?. UserOrganizationOrganization ==. organization ?. OrganizationId
  pure (user, organization)

allTickets :: SqlQuery (SqlExpr (Entity Ticket))
allTickets =
  from $ \ticket -> do
  pure ticket

-- | we can then combine sqlquery values:
-- However, this query is liable to runtime errors: if the organization is null, the ticket won't come up
joinOrgsAndTickets' :: SqlPersistT IO [(Entity User, Maybe (Entity Organization), Entity Ticket)]
joinOrgsAndTickets' =
  select $ do
    (user, morganization) <- usersWithOrganizationsQuery
    ticket <- allTickets
    where_ $
      just (ticket ^. TicketOrganization) ==. morganization ?. OrganizationId
    -- ossia: using overloaded labels:
    where_ $
      just (ticket ^. #organization) ==. morganization ?. OrganizationId
    pure (user, morganization, ticket)

-- FIXME: exercise: fixing the above query so tickets are also @Maybe@
-- this actually doesn't compile lol
-- joinOrgsAndTicketsWeird :: SqlPersistT IO [(Entity User, Maybe (Entity Organization), Maybe (Entity Ticket))]
-- joinOrgsAndTicketsWeird =
--   select $ do
--     (user, morganization) <- usersWithOrganizationsQuery
--     ticket <- allTickets
--     where_ $
--       just (ticket ^. #organization) ==. morganization ?. OrganizationId
--     where_ $ not_ $ isNothing $ morganization ?. OrganizationId
--     -- HACK: the tutorial said that we'd have to rewrite this function to account for tickets
--     -- but at first I wasn't sure how to do it, I think I know now???
--     -- but for this first attempt, I found the very strange toMaybe
--     -- https://hackage.haskell.org/package/esqueleto-3.5.4.0/docs/Database-Esqueleto-Experimental-ToMaybe.html
--      pure (user, morganization, toMaybe ticket)

-- | NOTE: not sure if this one _works_, I just shuffled things a bit until the types aligned, would be interested in seeing the actual results.
allTicketsForOrg :: SqlExpr (Value (Maybe OrganizationId)) -> SqlQuery (SqlExpr (Maybe (Entity Ticket)))
allTicketsForOrg mOrgId =
  from $ \(ticket `LeftOuterJoin` organization) -> do
  on $ (ticket ?. #organization) ==. mOrgId
  where_ $
    mOrgId ==. organization ?. OrganizationId
  pure ticket

-- NOTE: exercise: fixing the above query so tickets are also @Maybe@
joinOrgsAndTickets :: SqlPersistT IO [(Entity User, Maybe (Entity Organization), Maybe (Entity Ticket))]
joinOrgsAndTickets =
  select $ do
    (user, morganization) <- usersWithOrganizationsQuery
    ticket <- allTicketsForOrg $ morganization ?. OrganizationId
    pure (user, morganization, ticket)
