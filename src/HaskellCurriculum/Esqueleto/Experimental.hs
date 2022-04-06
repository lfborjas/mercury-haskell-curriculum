{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | From: https://github.com/MercuryTechnologies/esqueleto-training/blob/master/src/Experimental.hs

module HaskellCurriculum.Esqueleto.Experimental where

import Data.Traversable (for)
-- TODO: add 'shakespeare' to cabal
--import Text.Shakespeare.Text (st)
import Data.Text (Text)
import Data.Time (fromGregorian)
import HaskellCurriculum.Esqueleto.Models
import qualified Database.Persist as Persistent
import Database.Esqueleto.Experimental
import Database.Esqueleto.Legacy (SqlPersistT)
import HaskellCurriculum.Esqueleto.Legacy (usersWithOrganizations2)
import Database.Persist.Sql (SqlPersistT)
import Database.Esqueleto (where_)

-- | Rewriting 'get' with this syntax,
-- note that in Experimental, the @from@ clause takes a specification,
-- not a lambda that encompasses the rest of the query
get
  :: forall entity. (PersistRecordBackend entity SqlBackend)
  => Key entity
  -> SqlPersistT IO (Maybe (Entity entity))
get k = do
  result <-
    select $ do
      e <- from $ table @entity
      where_ $
        e ^. persistIdField ==. val k
      limit 1
      pure e
  pure $ case result of
    [x] -> Just x
    _ -> Nothing

-- | Reproducing the "get only users who belong to organizations" query
usersWithOrganizations1
  :: SqlPersistT IO [(Entity User, Entity Organization)]
usersWithOrganizations1 =
  select $ do
    (u :& _ :& o) <- from $
      -- NOTE: using type applications even though the type could be inferred, for clarity
      table @User
      `InnerJoin`
      table @UserOrganization
        -- NOTE: the @on@ clause is now part of the from specification
        `on` do
          \(u :& uo) -> u ^. #id ==. uo ^. #user
      `InnerJoin`
      table @Organization
        `on` do
          \(_ :& uo :& o) -> o ^. #id ==. uo ^. #organization
    pure (u,o)

-- | With the new from specifications, trying to write the below query with the faulty
-- logic (i.e. trying to get organization id when it might be NULL) is now a type error
usersWithOrganizations2
  :: SqlPersistT IO [(Entity User, Maybe (Entity Organization))]
usersWithOrganizations2 =
  select $ do
    (u :& _ :& o) <- from $
      table @User
      `LeftOuterJoin`
      table @UserOrganization
        `on` do
          \(u :& uo) ->
            -- NOTE: if we try to use the below line, which works fine in the legacy
            -- version, we now get a type mismatch: LeftOuterJoin necessitates maybes
            --u ^. #id ==. uo ^. #user
            just (u ^. #id) ==. uo ?. #user
      `LeftOuterJoin`
      table @Organization
        `on` do
          \(_ :& uo :& o) ->
            o ?. #id ==. uo ?. #organization
    pure (u, o)

-- | Subquery joins are new to Database.Esqueleto.Experimental
subqueryFrom
  :: SqlPersistT IO [(Entity User, Entity Customer)]
subqueryFrom =
  select $ do
    (user :& customer) <- from $
      table @User
      `InnerJoin`
      (do
          c <- from $ table @Customer
          where_ $
            c ^. CustomerJoined <=. val (fromGregorian 2020 05 30)
          pure c
      )
        `on` do
          \(u :& c) ->
            u ^. UserName ==. c ^. CustomerCallsign
    pure (user, customer)

-- The above query is of course equivalent to this simple JOIN (no subqueries),
-- though it's nice to see that one can join to both @table@s, and expressions
-- that return a relation, just like in regular SQL!
noSubqueryFrom :: SqlPersistT IO [(Entity User, Entity Customer)]
noSubqueryFrom =
  select $ do
    (user :& customer) <- from $
      table @User
      `InnerJoin`
      table @Customer
        `on` do
          \(u :& c) ->
            u ^. UserName ==. c ^. CustomerCallsign
    where_ $
      customer ^. CustomerJoined <=. val (fromGregorian 2020 05 30)
    pure (user, customer)

-- The ability to have expressions that can be joined to is that we
-- can factor out subqueries:
legacyCustomers :: SqlQuery (SqlExpr (Entity Customer))
legacyCustomers = do
  c <- from $ table @Customer
  where_ $ c ^. #joined <=. val (fromGregorian 2020 05 30)
  pure c

subqueryFromFactored :: SqlPersistT IO [(Entity User, Entity Customer)]
subqueryFromFactored = do
   select $ do
    (user :& customer) <- from $
      table @User
      `InnerJoin`
      legacyCustomers
        `on` do
          \(u :& c) ->
            u ^. UserName ==. c ^. CustomerCallsign
    pure (user, customer)

-- Another interesting consequence, though, is that instead of joining, multiple @from@
-- specifications may appear in a query, generating a CROSS JOIN, so this following query
-- generates this SQL:
-- SELECT user.*, customer.* from user,customer where user.name = customer.callsign and customer.joined <= '2020-05-30'
noSubqueryFromFactored :: SqlPersistT IO [(Entity User, Entity Customer)]
noSubqueryFromFactored =
  select $ do
    user <- from $ table @User
    cust <- legacyCustomers
    where_ $
      cust ^. CustomerJoined <=. val (fromGregorian 2020 05 30)
    pure (user, cust)

-- SET OPERATIONS

-- | Using unions to figure out the set of names and callsigns (which are meant to overlap)
-- note that due to the semantics of UNION in SQL, duplicates will be dropped. If we wanted
-- a bag and not a set, @unionAll_@ also exists.
-- NOTE: the example has separate functions for the UNION ALL case, and for factoring out
-- both subqueries. I consider this transcription to cover that ground, getting tired of typing.
-- NOTE: there's /another/ example where @intersect_@ is used to find the intersection instead
-- (i.e. instead of "all usernames and callsigns", "all usernames which are also callsigns.")
-- NOTE: and then there's even /yet another/ example for the EXCEPT construct, as provided
-- by @except_@, which does set difference, i.e. "names which are not also callsigns"
namesAndCallsigns :: SqlPersistT IO [Value Text]
namesAndCallsigns =
  select $ do
    -- NOTE: (luis) I extracted this to a 'let' because I kinda hate those tricky indents
    -- within parens in the example code
    let names = do
          u <- from $ table @User
          pure $ u ^. #name
        callsigns = do
          c <- from $ table @Customer
          pure $ c ^. #callsign
    nameOrCallsign <- from $
      names `union_` callsigns
    pure nameOrCallsign

-- CTEs

-- Another way of factoring out legacy users, by moving it to a CTE instead
-- of doing a CROSS JOIN.
-- Note that in purely practical terms, since we could already factor out @legacyCustomers@
-- to its own Haskell expression, joining against a CTE or a factored-out subquery
-- is pretty much the same -- using a CTE allows for memoization to happen vs.
-- factored-out expressions which may be inlined by the Haskell compiler;
-- due to query-planner magic, in most cases the difference is negligible.
-- /However/ recursive subqueries are rather nice, as demonstrated in the next example
-- REVIEW: did Matt mean memoization in Haskell too, or is this a PG thing?
subQueryFromCTE :: SqlPersistT IO [(Entity User, Entity Customer)]
subQueryFromCTE =
  select $ do
    customerQuery <-
      with $ do
        c <- from $ table @Customer
        where_ $ c ^. #joined <=. val (fromGregorian 2020 05 30)
        pure c
    u :& c <- from $
      table @User
      `InnerJoin`
      customerQuery
        `on` do
          \(u :& c) ->
            u ^. #name ==. c ^. #callsign
    pure (u, c)

-- ARBORIST SEGMENT
data TreeH = TreeH Int [TreeH]

sumTreeH :: TreeH -> Int
sumTreeH (TreeH value trees) =
  value + sum (map sumTreeH trees)

-- | The naive approach: get the tree rows from the DB, turn them into Haskell trees;
-- must account for the difference in representation
treeToTreeH :: TreeId -> SqlPersistT IO (Maybe TreeH)
treeToTreeH treeId = do
  mtree <- Persistent.get treeId
  for mtree $ \tree -> do
    getChildrenRecursively (Entity treeId tree)
  where
    getChildren :: TreeId -> SqlPersistT IO [Entity Tree]
    getChildren tid =
      Persistent.selectList [TreeParentId Persistent.==. Just tid] []

    getChildrenRecursively :: Entity Tree -> SqlPersistT IO TreeH
    getChildrenRecursively (Entity tid tree) = do
      childrenDatabase <- getChildren tid
      children <-
        for childrenDatabase $ \childTree ->
          getChildrenRecursively childTree
      pure $ TreeH (treeValue tree) children

-- | The above approach, from DB to Haskell, has a big problem: not only does it
-- result in several separate queries, it may end up with infinite recursion!
-- It's best to offload this to the DBMS:
sumTree :: TreeId -> SqlPersistT IO [Value (Maybe Int)]
sumTree treeId = do
  let parents = do
        t <- from $ table @Tree
        where_ $
          t ^. TreeId ==. val treeId
        pure t
      childrenFor self = do
        (_self :& children) <-
          from $
            self
            `InnerJoin`
            table @Tree
              `on` do
                \(s :& t) -> just (s ^. TreeId) ==. t ^. TreeParentId
        pure children
  select $ do
    treeQuery <- withRecursive parents union_ childrenFor
    tree <- from treeQuery
    pure $ sum_ $ tree ^. TreeValue
