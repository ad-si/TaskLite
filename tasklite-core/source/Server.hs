{-# LANGUAGE DataKinds #-}

module Server where

import Protolude (
  Applicative (pure),
  Eq ((==)),
  IO,
  Int,
  Maybe (Just, Nothing),
  Proxy (Proxy),
  Semigroup ((<>)),
  Text,
  const,
  putText,
  show,
  ($),
  (&),
  (||),
 )
import Protolude qualified as P

import Data.Aeson (Object)
import Data.Text qualified as T
import Network.Wai (Application, Middleware)
import Network.Wai.Application.Static (defaultWebAppSettings)
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setOnException,
  setPort,
 )
import Network.Wai.Middleware.Cors (
  cors,
  corsMethods,
  corsRequestHeaders,
  simpleCorsResourcePolicy,
  simpleMethods,
 )
import Network.Wai.Parse (
  defaultParseRequestBodyOptions,
  setMaxRequestFilesSize,
  setMaxRequestNumFiles,
 )
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Servant (
  Context (EmptyContext, (:.)),
  NoContent,
  err303,
  serveDirectoryWith,
 )
import Servant.API (
  Get,
  JSON,
  PlainText,
  Post,
  Raw,
  ReqBody,
  (:<|>) ((:<|>)),
  (:>),
 )
import Servant.HTML.Blaze (HTML)
import Servant.Multipart (
  MultipartOptions (generalOptions),
  Tmp,
  defaultMultipartOptions,
 )
import Servant.Server (Server)
import Servant.Server qualified as Servant
import WaiAppStatic.Types (
  LookupResult (LRFile, LRFolder, LRNotFound),
  StaticSettings (ssLookupFile),
  unsafeToPiece,
 )

import AirGQL.Config qualified as AirGQL (Config (maxDbSize), defaultConfig)
import AirGQL.ExternalAppContext (
  ExternalAppContext (
    ExternalAppContext,
    baseUrl,
    sqlite,
    sqliteLib
  ),
 )
import AirGQL.Lib (SQLPost)
import AirGQL.Servant.Database (
  apiDatabaseSchemaGetHandler,
  apiDatabaseVacuumPostHandler,
 )
import AirGQL.Servant.GraphQL (
  gqlQueryPostHandler,
  playgroundDefaultQueryHandler,
  readOnlyGqlPostHandler,
 )
import AirGQL.Servant.SqlQuery (sqlQueryPostHandler)
import AirGQL.Types.SchemaConf (SchemaConf (pragmaConf), defaultSchemaConf)
import AirGQL.Types.SqlQueryPostResult (SqlQueryPostResult)
import AirGQL.Types.Types (GQLPost)
import Config as TaskLite (Config)
import Lib (getDbPath)


{- FOURMOLU_DISABLE -}
-- ATTENTION: Order of handlers matters!
type PlatformAPI =
  -- sqlQueryPostHandler
  "sql"
          :> ReqBody '[JSON] SQLPost
          :> Post '[JSON] SqlQueryPostResult

  -- Redirect to GraphiQL playground
  -- redirectToPlayground
  :<|> "graphql" :> Get '[HTML] NoContent

  -- gqlQueryPostHandler
  :<|> "graphql"
          :> ReqBody '[JSON] GQLPost
          :> Post '[JSON] Object

  -- readOnlyGqlPostHandler
  :<|> "readonly" :> "graphql"
          :> ReqBody '[JSON] GQLPost
          :> Post '[JSON] Object

  -- playgroundDefaultQueryHandler
  :<|> "playground" :> "default-query"
          :> Get '[PlainText] Text

  -- apiDatabaseSchemaGetHandler
  :<|> "schema"
          :> Get '[PlainText] Text

  -- apiDatabaseVacuumPostHandler
  :<|> "vacuum"
          :> Post '[JSON] Object

{- FOURMOLU_ENABLE -}


platformAPI :: Proxy (PlatformAPI :<|> Raw)
platformAPI = Proxy


redirectToPlayground :: Servant.Handler a
redirectToPlayground =
  P.throwError
    err303
      { Servant.errHeaders =
          [("Location", P.encodeUtf8 "/graphiql")]
      }


platformServer :: ExternalAppContext -> Text -> Server PlatformAPI
platformServer ctx dbPath = do
  sqlQueryPostHandler defaultSchemaConf.pragmaConf dbPath
    :<|> redirectToPlayground
    :<|> gqlQueryPostHandler defaultSchemaConf dbPath
    :<|> readOnlyGqlPostHandler dbPath
    :<|> playgroundDefaultQueryHandler dbPath
    :<|> apiDatabaseSchemaGetHandler ctx dbPath
    :<|> apiDatabaseVacuumPostHandler dbPath


platformApp :: ExternalAppContext -> Text -> Application
platformApp ctx dbPath = do
  let
    maxFileSizeInByte :: Int = AirGQL.defaultConfig.maxDbSize

    multipartOpts :: MultipartOptions Tmp
    multipartOpts =
      (defaultMultipartOptions (Proxy :: Proxy Tmp))
        { generalOptions =
            setMaxRequestNumFiles 1 $
              setMaxRequestFilesSize
                (P.fromIntegral maxFileSizeInByte)
                defaultParseRequestBodyOptions
        }

    context :: Context '[MultipartOptions Tmp]
    context =
      multipartOpts :. EmptyContext

    webappServerSettings :: Text -> StaticSettings
    webappServerSettings root =
      let
        webAppSettings = defaultWebAppSettings $ T.unpack root

        lookup pieces = do
          lookupResult <- ssLookupFile webAppSettings pieces
          case lookupResult of
            LRFile file -> pure $ LRFile file
            LRFolder folder -> pure $ LRFolder folder
            LRNotFound ->
              ssLookupFile
                webAppSettings
                [unsafeToPiece "index.html"]
      in
        webAppSettings{ssLookupFile = lookup}

  Servant.serveWithContext platformAPI context $
    platformServer ctx dbPath
      :<|> serveDirectoryWith (webappServerSettings "tasklite-app/build")


corsMiddleware :: Middleware
corsMiddleware =
  let
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type", "Authorization"]
        , corsMethods = "PUT" : simpleMethods
        }
  in
    cors (const $ Just policy)


-- | Uses AirGQL to provide a GraphQL endpoint at /graphql
startServer :: AirGQL.Config -> TaskLite.Config -> IO (Doc AnsiStyle)
startServer _airgqlConf taskliteConf = do
  let
    port :: Int = 7458

    runWarp =
      runSettings $
        defaultSettings
          & setPort port
          & setOnException
            ( \_ exception -> do
                let exceptionText :: Text = show exception
                if (exceptionText == "Thread killed by timeout manager")
                  || ( exceptionText
                         == "Warp: Client closed connection prematurely"
                     )
                  then pure ()
                  else do
                    putText exceptionText
            )

    ctx =
      ExternalAppContext
        { sqlite = ""
        , sqliteLib = Nothing
        , baseUrl = ""
        }

  putText $
    "\n\n"
      <> "Starting GraphQL server at http://localhost:"
      <> show port
      <> "/graphql"

  dbPath <- P.liftIO $ getDbPath taskliteConf

  runWarp $ corsMiddleware $ platformApp ctx (T.pack dbPath)

  pure P.mempty
