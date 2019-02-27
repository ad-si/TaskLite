{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies #-}

module Database where

import Protolude as P hiding (get, put)

import Crypto.BCrypt (validatePassword)
import qualified Control.Monad.State as State
import Data.Acid as Acid
import Data.SafeCopy

import DbIdea
import DbUser
import Types
import Network.HTTP.Types.Status


$(deriveSafeCopy 0 'base ''Status)


data Database = Database [DbUser] [DbIdea]

$(deriveSafeCopy 0 'base ''Database)

addUser :: DbUser -> Update Database ()
addUser user = do
  Database users ideas <- State.get
  State.put $ Database (user : users) ideas


setTokenWhere :: RefreshToken -> RefreshToken -> Update Database ()
setTokenWhere searchForToken replacementToken = do
  Database users ideas <- State.get

  State.put $ Database
    (fmap
      (\user -> if DbUser.refresh_token user == Just searchForToken
          then user {DbUser.refresh_token = Just replacementToken}
          else user
      )
      users)
    ideas


getUserByToken :: RefreshToken -> Query Database (Either Text DbUser)
getUserByToken refreshToken = do
  Database users _ <- ask
  pure $ note
    "A user with the provided refresh token does not exist"
    (P.find
      (\u -> DbUser.refresh_token u == Just refreshToken)
      users)


deleteUserByToken :: RefreshToken -> Update Database (Maybe ())
deleteUserByToken refreshToken = do
  Database users ideas <- State.get
  let newUsers = P.filter
        (\u -> DbUser.refresh_token u /= Just refreshToken)
        users

  if length users /= length newUsers
  then do
    State.put $ Database newUsers ideas
    pure $ Just ()
  else
    pure Nothing


logoutByEmailAndToken ::
  Text -> RefreshToken -> Update Database (Either (Status, Text) DbUser)
logoutByEmailAndToken emailAddress searchForToken = do
  Database users ideas <- State.get

  let
    userResult = note
      (notFound404, "A user with the provided refresh token does not exist")
      (P.find
        (\u -> DbUser.refresh_token u == Just searchForToken)
        users)

    otherUsers = P.filter
        (\u -> DbUser.refresh_token u /= Just searchForToken)
        users

    finalResult = userResult >>= (\dbUser ->
      if DbUser.email dbUser /= emailAddress
      then Left (forbidden403, "No rights to log this user out")
      else Right dbUser
      )

  case finalResult of
    Left error -> pure $ Left error
    Right dbUser -> do
      let updatedUser = dbUser {DbUser.refresh_token = Nothing}
      State.put $ Database (updatedUser : otherUsers) ideas
      pure $ Right dbUser


logUserIn ::
  Text -> Text -> RefreshToken -> Update Database (Either Text DbUser)
logUserIn email password newToken = do
  Database users ideas <- State.get
  let
    userMaybe = P.find (\u -> DbUser.email u == email) users
    otherUsers = P.filter (\u -> DbUser.email u /= email) users

  case userMaybe of
    Nothing -> pure $ Left "User does not exist"
    Just dbUser -> do
      let
        hash_ = P.encodeUtf8 $ DbUser.password_hash dbUser
        password_ = P.encodeUtf8 password

      if not $ validatePassword hash_ password_
      then pure $ Left "Invalid password"
      else
        if isJust $ DbUser.refresh_token dbUser
        then pure $ Right dbUser
        else do
          let newUser = dbUser {DbUser.refresh_token = Just newToken}
          State.put $ Database (newUser : otherUsers) ideas
          pure $ Right newUser


getUserByEmail :: Text -> Query Database (Maybe DbUser)
getUserByEmail emailAddress = do
  Database users _ <- ask
  let userMaybe = P.find (\u -> DbUser.email u == emailAddress) users
  pure userMaybe


getUsers :: Query Database [DbUser]
getUsers = do
  Database users _ <- ask
  pure users



addIdea :: DbIdea -> Update Database ()
addIdea idea = do
  Database users ideas <- State.get
  State.put $ Database users (idea : ideas)


updateIdea :: Text -> DbIdea -> Update Database (Maybe ())
updateIdea id idea = do
  Database users ideas <- State.get
  let otherIdeas = P.filter
        (\idea_ -> DbIdea.id idea_ /= id)
        ideas

  if length ideas /= length otherIdeas
  then do
    State.put $ Database users (idea : ideas)
    pure $ Just ()
  else
    pure Nothing


getIdeas :: Query Database [DbIdea]
getIdeas = do
  Database _ ideas <- ask
  pure ideas


getIdeasByEmail :: Text -> Query Database [DbIdea]
getIdeasByEmail emailAddress = do
  Database _ ideas <- ask
  pure $ P.filter
    (\idea_ -> DbIdea.created_by idea_ == emailAddress)
    ideas


deleteIdeaIfBy :: Text -> Text -> Update Database (Either (Status, Text) ())
deleteIdeaIfBy emailAddress id = do
  Database users ideas <- State.get
  let
    ideaMaybe = P.find (\idea -> DbIdea.id idea == id) ideas
    otherIdeas = P.filter (\idea -> DbIdea.id idea /= id) ideas

  case ideaMaybe of
    Nothing ->
      pure $ Left (notFound404, "Idea with the provided id is not available")
    Just idea ->
      if DbIdea.created_by idea /= emailAddress
      then do
        pure $ Left (forbidden403, "No rights to delete this idea")
      else do
        State.put $ Database users otherIdeas
        pure $ Right ()


$(makeAcidic ''Database
  [ 'addUser
  , 'setTokenWhere
  , 'getUserByToken
  , 'logoutByEmailAndToken
  , 'deleteUserByToken
  , 'logUserIn
  , 'getUserByEmail
  , 'getUsers
  , 'addIdea
  , 'updateIdea
  , 'getIdeas
  , 'getIdeasByEmail
  , 'deleteIdeaIfBy
  ])
