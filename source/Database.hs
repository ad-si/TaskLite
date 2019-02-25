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


getUserByToken :: RefreshToken -> Query Database (Maybe DbUser)
getUserByToken refreshToken = do
  Database users _ <- ask
  pure $ P.find
    (\u -> DbUser.refresh_token u == Just refreshToken)
    users


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


logoutUserByToken :: RefreshToken -> Update Database (Maybe ())
logoutUserByToken searchForToken = do
  Database users ideas <- State.get

  let
    userMaybe = P.find
      (\u -> DbUser.refresh_token u == Just searchForToken)
      users
    otherUsers = P.filter
        (\u -> DbUser.refresh_token u /= Just searchForToken)
        users

  case userMaybe of
    Nothing -> pure Nothing
    Just user -> do
      let updatedUser = user {DbUser.refresh_token = Nothing}
      State.put $ Database (updatedUser : otherUsers) ideas
      pure $ Just ()


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


deleteIdea :: Text -> Update Database (Either Text ())
deleteIdea id = do
  Database users ideas <- State.get
  let
    newIdeas = P.filter
      (\idea -> DbIdea.id idea /= id)
      ideas

  if length ideas /= length newIdeas
  then do
    State.put $ Database users newIdeas
    pure $ Right ()
  else
    pure $ Left "Idea with the provided id is not available"


$(makeAcidic ''Database
  [ 'addUser
  , 'setTokenWhere
  , 'getUserByToken
  , 'logoutUserByToken
  , 'deleteUserByToken
  , 'logUserIn
  , 'getUserByEmail
  , 'getUsers
  , 'addIdea
  , 'updateIdea
  , 'getIdeas
  , 'getIdeasByEmail
  , 'deleteIdea
  ])
