{-# LANGUAGE DataKinds #-}

module Server where

import Protolude (
  Applicative (pure),
  Bool (False),
  ByteString,
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
  (<&>),
  (||),
 )
import Protolude qualified as P

import Data.Aeson (Object)
import Data.Text qualified as T
import Network.HTTP.Types (hContentType, status403)
import Network.Wai (
  Application,
  Middleware,
  requestHeaderHost,
  responseLBS,
 )
import Network.Wai.Application.Static (defaultWebAppSettings)
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setHost,
  setOnException,
  setPort,
 )
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsMethods, corsOrigins, corsRequestHeaders),
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
import System.Directory (doesDirectoryExist)
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
import AirGQL.Lib (SQLPost, readOnly)
import AirGQL.Servant.Database (
  apiDatabaseSchemaGetHandler,
  apiDatabaseVacuumPostHandler,
 )
import AirGQL.Servant.GraphQL (
  gqlQueryPostHandler,
  playgroundDefaultQueryHandler,
 )
import AirGQL.Servant.SqlQuery (sqlQueryPostHandler)
import AirGQL.Types.SchemaConf (
  SchemaConf (accessMode, pragmaConf),
  defaultSchemaConf,
 )
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


platformServer :: ExternalAppContext -> P.FilePath -> Server PlatformAPI
platformServer ctx dbPath = do
  let dbId = T.pack dbPath
  sqlQueryPostHandler defaultSchemaConf.pragmaConf dbPath
    :<|> redirectToPlayground
    :<|> gqlQueryPostHandler defaultSchemaConf dbPath dbId
    :<|> gqlQueryPostHandler
      defaultSchemaConf{accessMode = readOnly}
      dbPath
      dbId
    :<|> playgroundDefaultQueryHandler dbPath dbId
    :<|> apiDatabaseSchemaGetHandler ctx dbPath
    :<|> apiDatabaseVacuumPostHandler dbPath


platformApp :: ExternalAppContext -> P.FilePath -> Application
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
      :<|> serveDirectoryWith (webappServerSettings webappDir)


{-| Directory of the built web app (`make build` in tasklite-webapp),
relative to the working directory of the server
-}
webappDir :: Text
webappDir = "tasklite-webapp/build"


{-| Only allow cross-origin requests from the web app
(served by this server or by its development server on port 7459).
Browsers also send an `Origin` header for same-origin POST requests,
therefore the server's own origins must be allowed as well.
-}
corsMiddleware :: Int -> Middleware
corsMiddleware port =
  let
    allowedOrigins :: [ByteString]
    allowedOrigins = do
      host <- ["localhost", "127.0.0.1"]
      originPort <- [port, 7459]
      pure $ "http://" <> host <> ":" <> show originPort

    policy =
      simpleCorsResourcePolicy
        { corsOrigins = Just (allowedOrigins, False)
        , corsRequestHeaders = ["Content-Type", "Authorization"]
        , corsMethods = "PUT" : simpleMethods
        }
  in
    cors (const $ Just policy)


{-| Reject requests whose `Host` header does not refer to this machine.
Prevents DNS rebinding attacks, where a website changes its domain
to resolve to 127.0.0.1 and thereby circumvents the CORS policy.
-}
hostCheckMiddleware :: Int -> Middleware
hostCheckMiddleware port app request respond =
  let
    allowedHosts :: [ByteString]
    allowedHosts =
      ["localhost", "127.0.0.1"] <&> \host -> host <> ":" <> show port
  in
    if P.maybe False (`P.elem` allowedHosts) (requestHeaderHost request)
      then app request respond
      else
        respond $
          responseLBS
            status403
            [(hContentType, "text/plain; charset=utf-8")]
            "Forbidden: Invalid Host header"


-- | Uses AirGQL to provide a GraphQL endpoint at /graphql
startServer :: AirGQL.Config -> TaskLite.Config -> IO (Doc AnsiStyle)
startServer _airgqlConf taskliteConf = do
  let
    port :: Int = 7458

    runWarp =
      runSettings $
        defaultSettings
          -- Only accept connections from this machine
          & setHost "127.0.0.1"
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

  webappExists <- doesDirectoryExist $ T.unpack webappDir
  if webappExists
    then putText $ "Serving web app at http://localhost:" <> show port
    else
      P.putErrText $
        "Web app is not served, because directory \""
          <> webappDir
          <> "\" does not exist in the current working directory"

  dbPath <- P.liftIO $ getDbPath taskliteConf

  runWarp $
    hostCheckMiddleware port $
      corsMiddleware port $
        platformApp ctx dbPath

  pure P.mempty
