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
import Data.Foldable (traverse_)
import Database.Persist.Sql (SqlPersistT)
import Database.Esqueleto (countRows)
import Database.Persist.Postgresql (SqlPersistT)

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

-- | Demonstration of a CASE expression translated to Esqueleto
-- NOTE: I had to add a few type annotations, not sure how it works
-- fine in the example?
sillyExample :: SqlExpr (Value String)
sillyExample =
  case_
    [ when_ (val (2 :: Int) ==. val 1)
      then_ (val "hello" )
    , when_ (val ("a" :: String) ==. val "b")
      then_ (val "goodbye")
    ]
    $ else_ (val "nope")


-- Now that we know @case_@, let's write a new helper: @if_@
if_
  :: PersistField a
  => SqlExpr (Value Bool)
  -- ^ always good form to work in the SQL realm vs. Haskell values:
  -- you can always go from Haskell to SQL with @val@, but it's hard to stay
  -- in the Query monad when trying to convert the other way
  -> SqlExpr (Value a)
  -- ^ Always remember to "tag" types with Value, to mean a column vs. a raw type
  -> SqlExpr (Value a)
  -> SqlExpr (Value a)
if_ cond ifTrue ifFalse =
  case_
    [ when_ cond
      then_ ifTrue
    ]
    $ else_ ifFalse


------------------------------------------------------------------------------------
-- EXERCISES
------------------------------------------------------------------------------------

-- | Select all tickets that belong to an org
ticketsForOrganization :: SqlExpr (Value OrganizationId) -> SqlQuery (SqlExpr (Entity Ticket))
ticketsForOrganization orgId =
  from $ \ticket -> do
  where_ $
    ticket ^. #organization ==. orgId
  pure ticket

-- | Join orgs with tickets
organizationsWithTickets :: SqlQuery (SqlExpr (Entity Organization), SqlExpr (Entity Ticket))
organizationsWithTickets =
  from $ \(organization `InnerJoin` ticket) -> do
  on $ ticket ^. #organization ==. organization ^. #id
  pure (organization, ticket)

-- | Tickets where a user has commented
userCommentedOnTickets :: SqlExpr (Value UserId) -> SqlQuery (SqlExpr (Entity Ticket))
userCommentedOnTickets uid =
  from $ \(user `InnerJoin` ticketComment `InnerJoin` ticket) -> do
  -- NOTE: interesting that I was forced to use the non-overloadedlabels version here, got ambiguity complaints otherwise
  on $ ticketComment ^. TicketCommentAuthor  ==. user ^. UserId
  -- NOTE: but this line worked fine
  on $ ticketComment ^. #ticket ==. ticket ^. #id
  where_ $ user ^. #id ==. uid
  pure ticket

-------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AGGREGATIONS
-- They are sad in Esqueleto.
-------------------------------------------------------------------------------------------------------------------------------------------------------------

-- NOTE: the aggregation is a @Maybe@ because the result set may be empty
totalSold :: SqlPersistT IO Int
totalSold =
  fmap conv $
  select $
  from $ \sale -> do
  pure $ sum_ (sale ^. SaleAmount)
  where
    conv :: [Value (Maybe Int)] -> Int
    conv values =
      case values of
        [(Value (Just i))] -> i
        _ -> 0


-- EXERCISES AGAIN!

-- | Write a fromMaybe. Hint: COALESCE?
fromMaybe_
  :: PersistField a
  => SqlExpr (Value a)
  -- ^ default
  -> SqlExpr (Value (Maybe a))
  -- ^ actual value
  -> SqlExpr (Value a)
  -- ^ coalesce(value,default)
fromMaybe_ onNothing mval =
  -- NOTE: could also have been: coalesce [mval, onNothing]
  -- since the docs say that it'll return the first not-null val
  -- HACK: I think I cheated here... but, like... how else to do it??
  -- maybe with like a case?
  coalesceDefault [mval] onNothing

selectOne' :: SqlSelect a r => SqlQuery a -> SqlPersistT IO (Maybe r)
selectOne' q =
  -- NOTE: the limit 1 is an idea from esqueleto itself:
  -- https://hackage.haskell.org/package/esqueleto-3.5.4.0/docs/src/Database.Esqueleto.Internal.Internal.html#selectOne
  listToMaybe <$> (select $ limit 1 >> q)

-- NOTE: I /think/ this was the expected approach?
selectOneMaybe
  :: PersistField a
  => SqlQuery (SqlExpr (Value (Maybe a)))
  -> SqlPersistT IO (Maybe a)
selectOneMaybe query = do
  res <- selectOne query
  pure $ case res of
    Just (Value (Just a)) -> Just a
    _ -> Nothing

-- TODO: exercise: deal with Entity/Value tuples. Maybe a new ADT?

-- | Return both the amount sold and count of all sales
sumAndCount
  :: SqlPersistT IO (Maybe (Int, Int))
sumAndCount =
  -- NOTE: using conv <$> messes with the precedence, causing a type
  -- error -- I think because it tries to fmap over select before the
  -- rest of the expression?
  fmap conv $
  select $
  from $ \sale -> do
  pure
    ( fromMaybe_ (val 0) $ sum_ (sale ^. SaleAmount)
    , countRows
    )
  where
    conv :: [(Value Int, Value Int)] -> Maybe (Int, Int)
    conv = coerce . listToMaybe

-- | Some buggy behavior: being forced to group by a column
-- if one wants it in the result set (which means some spurious groupings
-- sometimes.) Esqueleto does /not/ enforce it, so it can result in
-- a runtime error
buyerWithAmountBad :: SqlPersistT IO [(Value CustomerId, Value (Maybe Int))]
buyerWithAmountBad =
  select $
  from $ \sales -> do
  pure
    -- NOTE: notice how we had to specify /at least/ the first of these fields,
    -- vs. using the overloaded label, otherwise it would've been ambiguous.
    ( sales ^. SaleBuyer
    , sum_ $ sales ^. #amount
    )

buyerWithAmountGood :: SqlPersistT IO [(Value CustomerId, Value (Maybe Int))]
buyerWithAmountGood =
  select $
  from $ \sale -> do
  -- NOTE: to fix the "sql will complain that buyer wasn't used in grouping"
  groupBy $ sale ^. SaleBuyer
  pure
    ( sale ^. #buyer
    , sum_ $ sale ^. #amount
    )

buyerWithAmountGood2 :: SqlPersistT IO [(Value CustomerId, Value (Maybe Int))]
buyerWithAmountGood2 =
  select $
  from $ \sale -> do
  -- NOTE: a little bit of deduplication since we're in do notation
  let buyer = sale ^. SaleBuyer
  groupBy buyer
  pure
    ( buyer
    , sum_ $ sale ^. #amount
    )

-- EXISTS and sub-selects for more complex queries:
ticketsWithNoAuthorComments
  :: SqlQuery (SqlExpr (Entity Ticket))
ticketsWithNoAuthorComments =
  from $ \ticket -> do
  where_ $
    ticket ^. TicketCreator `notIn` do
      subSelectList $
        from $ \ticketComment -> do
        where_ $
          ticketComment ^. TicketCommentTicket ==. ticket ^. TicketId
        pure $
          ticketComment ^. TicketCommentAuthor
  -- NOTE: this is exactly the same as above, but using @NOT EXISTS@
  where_ $ notExists $
    from $ \ticketComment -> do
    where_ $
      ticketComment ^. TicketCommentTicket ==. ticket ^. TicketId
    where_ $
      ticketComment ^. #author ==. ticket ^. #creator

  pure ticket

-- | EXERCISE! Organizations for which no tickets exist
orgsNoTickets :: SqlQuery (SqlExpr (Entity Organization))
orgsNoTickets =
  from $ \organization -> do
  where_ $ notExists $
    from $ \ticket -> do
    where_ $
      ticket ^. TicketOrganization  ==. organization ^. OrganizationId
  pure organization

-- | EXERCISE! Tickets where the creator is not in the org
ticketsWithCreatorNotInOrg :: SqlQuery (SqlExpr (Entity Ticket))
ticketsWithCreatorNotInOrg =
  from $ \(ticket `InnerJoin` organization) -> do
  on $ ticket ^. TicketOrganization ==. organization ^. OrganizationId
  where_ $ notExists $
    from $ \userOrganization -> do
    where_ $
      userOrganization ^. UserOrganizationUser ==. ticket ^. TicketCreator
    where_ $
      userOrganization ^. UserOrganizationOrganization ==. organization ^. OrganizationId
  pure ticket
