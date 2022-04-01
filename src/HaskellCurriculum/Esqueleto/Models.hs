-- | From: https://github.com/MercuryTechnologies/esqueleto-training
-- https://github.com/MercuryTechnologies/esqueleto-training/blob/7002e3f21d11ce9c59f932c07abe56121a821a3a/src/Models.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HaskellCurriculum.Esqueleto.Models where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable
import Data.Time
import Data.Traversable
import Database.Persist.Sql
import Database.Persist.TH
import Data.List.NonEmpty (NonEmpty(..))

import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT)



share
  [mkMigrate "migrateAll", mkPersist sqlSettings]
  [persistLowerCase|
--| documentation blocks
--| always start with a pipe
User
  name Text
  age Int
  deriving Show

-- Explicit Id for org: take care to provide a default
Organization
  Id Text default=uuid_generate_v1mc()
  name Text

-- Using @callsign@ as a "natural" primary key, no @id@ will be generated
Customer
  callsign Text
  joined Day
  Primary callsign

-- by default, FKs have an @on delete RESTRICT@ behavior
-- note the use of @Primary@ foor determining a composite PK
UserOrganization
  user UserId
  organization OrganizationId OnDelete Cascade OnUpdate Cascade
  Primary user organization

Ticket
  name Text
  organization OrganizationId
  creator UserId
  assignee UserId Maybe
  description Text
  deriving Eq Show Ord

TicketComment
  ticket TicketId OnDelete Cascade
  author UserId OnDelete Cascade
  body Text

Sale
  amount Int
  time UTCTime
  buyer CustomerId
  thing Text
  seller Organization

-- | example of a recursive table, to represent a rose tree
Tree
  parentId TreeId Maybe
  value Int
|]

-- now, we hand-roll some stuff:

data User2 = User2
  { user2Name :: !Text
  , user2Age :: !Int
  }

-- NOTE: not in the tutorial (only @Lens'@ is provided), but couldn't resist
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
lens' :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens' getter setter f s = setter s <$> f (getter s)

-- | need an instance of @PersistEntity@ to deal with the whole table
instance PersistEntity User2 where

  -- An associated data type determines the primary key
  newtype Key User2 = User2Key {unUser2Key :: BackendKey SqlBackend}
    deriving newtype (Show, Eq, Read, Ord, ToJSON, FromJSON, PersistField)

  -- how to convert a key to a list of persistent scalars to insert into the DB
  -- it's a list to account for composite keys that have more than one col
  keyToValues :: Key User2 -> [PersistValue]
  keyToValues u2key = [toPersistValue (unUser2Key u2key)]

  -- dual of the above
  keyFromValues :: [PersistValue] -> Either Text (Key User2)
  keyFromValues pvs =
    case pvs of
      [pv] -> do
        backendKey <- fromPersistValue pv
        pure $ User2Key backendKey

  -- associated type to determine which persistent backend this table "speaks" to
  type PersistEntityBackend User2 = SqlBackend

  -- EntityField allows us to talk about the fields of the record, allowing us to
  -- encode the model each column belongs to, alongside its own type
  data EntityField User2 typ where
    User2Id :: EntityField User2 (Key User2)
    User2Name :: EntityField User2 Text
    User2Age :: EntityField User2 Int

  -- method that allows talking about this model's key polymorphically
  persistIdField :: EntityField User2 (Key User2)
  persistIdField = User2Id

  -- Matt says in the docs that this method should morally be @toPersistFields :: Entity User2 -> [PersistValue]@
  -- but I believe that would imply that the primary key is provided, which is not always the case? i.e. one can insert
  -- a User2 without having the pk yet, and thus not having an @Entity@, per-se, yet.
  -- Anyway, it renders an entity into database values
  toPersistFields :: User2 -> [SomePersistField]
  toPersistFields User2{user2Name, user2Age} =
    [ SomePersistField user2Name
    , SomePersistField user2Age
    ]

  fromPersistValues :: [PersistValue] -> Either Text User2
  fromPersistValues pvals =
    case pvals of
      [pvName, pvAge] -> do
        name <- fromPersistValue pvName
        age <- fromPersistValue pvAge
        pure User2
          { user2Name = name
          , user2Age = age
          }
      _ -> Left "Failed to parse User2"

  -- Regular sum type listing each of the unique constraints, no magic here.
  data Unique User2 = User2UniqueName Text

  -- line up the above Unique data type with the field names on both the Haskell and DB sides
  -- (need an explicit mapping since they may differ)
  persistUniqueToFieldNames :: Unique User2 -> NonEmpty (FieldNameHS, FieldNameDB)
  persistUniqueToFieldNames uniq =
    case uniq of
      User2UniqueName _ -> pure (FieldNameHS "name", FieldNameDB "name")

  -- get all unique keys for an entity
  persistUniqueKeys :: User2 -> [Unique User2]
  persistUniqueKeys u2 =
    [User2UniqueName (user2Name u2)]

  -- dual of the above
  persistUniqueToValues :: Unique User2 -> [PersistValue]
  persistUniqueToValues u2 =
    case u2 of
      User2UniqueName n -> [toPersistValue n]

  -- lil known one: can be used to create lenses for fields into/out of the entity
  fieldLens :: EntityField User2 a -> Lens'  (Entity User2) a
  fieldLens ef =
    case ef of
      User2Id -> error "wonder how this _is_ written?"
      _ -> error "not gonna implement all of them"

  -- see: https://hackage.haskell.org/package/persistent-2.13.3.3/docs/Database-Persist-EntityDef.html
  entityDef :: proxy User2 -> EntityDef
  entityDef _ = error "this is one of the biggest boilerplate ones"

  -- see: https://hackage.haskell.org/package/persistent-2.13.3.3/docs/Database-Persist-FieldDef-Internal.html#t:FieldDef
  persistFieldDef :: EntityField User2 typ -> FieldDef
  persistFieldDef _ = error "also another big boilerplaty one"

-- this alias is also gnerated:

type User2Id = Key User2

-- Extra treat! OverloadedLabels to be able to use a shortened version
-- of field identifiers!!
-- e.g. @#id@ instead of @OrganizationId@, and they're polymorphic
-- so the correct record+type will be inferred from context by the type checker.

instance SymbolToField "name" User2 Text where
  symbolToField = User2Name

instance SymbolToField "age" User2 Int where
  symbolToField = User2Age

instance SymbolToField "id" User2 User2Id where
  symbolToField = User2Id

-- | Examples of querying

-- NOTE(luis) added this lil function for convenience, and to illustrate some stuff
-- NOTE:  in the example, we hadn't derived @Show@ for user, so I added it
-- to the TH
printUser :: Entity User -> ReaderT SqlBackend IO ()
printUser (Entity userId user) = liftIO $ print (userId, user)

-- | Ex. 1 -- get all @User@ entities out of the DB
-- NOTE: I split it a little to document type signatures better
example1 :: SqlPersistT IO ()
example1 = do
  users <- getUsers
  forM_ users $ \u@(Entity _userKey _user) ->
    printUser u
    where
    -- NOTE: we're using type applications for clarity,
    -- but it's usually inferred.
    getUsers :: ReaderT SqlBackend IO [Entity User]
    getUsers = selectList @User [] []


-- | Users named bob at least 50 y/o
example2 ::SqlPersistT IO ()
example2 = do
  users <- getBobs
  traverse_ printUser users
  where
    getBobs:: ReaderT SqlBackend IO [Entity User]
    getBobs =
      selectList @User
        [UserName ==. "Bob", #age >=. 50]
        [Asc UserAge, LimitTo 50]
    -- NOTE: exercise: /customers/ joining after a certain day
    getUsersAfter :: Day -> ReaderT SqlBackend IO [Entity Customer]
    getUsersAfter dt =
      -- NOTE: notice how the overloaded labels are part of what TH generated for this!
      selectList @Customer
        [#joined >=. dt]
        [Desc CustomerJoined]

-- | getting a user by @Key rec@
example3 :: SqlPersistT IO ()
example3 = do
  muser <- getUser theUserId
  case muser of
    Nothing -> liftIO $ putStrLn "Not found!"
    -- NOTE: this diverges from the example, but I wanted to see how to "rebuild"
    -- an entity when all we have is the underlying record
    -- Note that @UserKey :: BackendKey SqlBackend -> Key User@, and @UserId = Key User@,
    -- the constructor coming from the associated newtype generated by the TH.
    Just us -> printUser (Entity theUserId us)

  where
    theUserId :: UserId
    theUserId = UserKey (32 :: BackendKey SqlBackend)
    -- NOTE: that UserId is an alias for Key User, which in turn is constructed from
    -- a @BackendKey SqlBackend@, which in the case of pg is an Int64 (see annotation above)
    getUser :: Key User -> ReaderT SqlBackend IO (Maybe User)
    getUser = get

-- | Get a customer by PK, which in this case is the @callsign@, also, a more succinct
-- iteration of the above without all the clarifying type signatures
example4 :: SqlPersistT IO ()
example4 = do
  mcustomer <- get (CustomerKey "i-am-a-callsign")
  liftIO $ case mcustomer of
    Nothing -> putStrLn "Oh noes!"
    Just cus -> putStrLn $ "The customer joined on" <> show (customerJoined cus)

-- | Getting by composite primary key
example5 :: UserId -> OrganizationId -> SqlPersistT IO (Maybe UserOrganization)
-- NOTE: the difference between the @UserOrganizationKey@ and the @UserId@ type alias
-- is that the former takes the components, and the latter is "fully saturated"
example5 userId organizationId = get (UserOrganizationKey userId organizationId)

-- | motivating example for esqueleto: getting all orgs with their users:
-- pure persistent only affords us the N+1 style join.
badJoin :: SqlPersistT IO [(Entity Organization, [Entity User])]
badJoin = do
  organizations <- selectList @Organization [] []
  for organizations $ \entityOrganization -> do
    -- NOTE: @entityKey@ will take an entity and return its key
    let orgId = entityKey entityOrganization
    userOrgs <- selectList @UserOrganization [#organization ==. orgId] []
    -- NOTE: @userOrganizationUser@ is the "user_id" column, and @entityVal@ can
    -- extract the record out of an @Entity id record@
    let userIds = map (userOrganizationUser . entityVal) userOrgs
    users <- for userIds $ \userId -> do
      muser <- get userId
      case muser of
        Nothing -> error "Did we somehow delete the user but not the join-table row?"
        Just user -> pure $ Entity userId user
    pure (entityOrganization, users)

-- Bonus: trying to make the above query slightly less bad
lessBadJoin :: SqlPersistT IO [(Entity Organization, [Entity User])]
lessBadJoin = do
  organizations <- selectList @Organization [] []
  for organizations $ \entityOrganization -> do
    let orgId = entityKey entityOrganization
    userOrgs <- selectList @UserOrganization [#organization ==. orgId] []
    let userIds = map (userOrganizationUser . entityVal) userOrgs
    users <- selectList @User [#id <-. userIds] []
    pure (entityOrganization, users)
