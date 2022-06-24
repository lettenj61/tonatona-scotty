module Tonatona.Scotty (Config(..), run) where

import           RIO

import           Data.Default                              (def)
import           Network.Wai                               (Middleware)
import           Network.Wai.Handler.Warp                  (Port)
import qualified Network.Wai.Handler.Warp                  as Warp
import           Network.Wai.Middleware.RequestLogger      (RequestLoggerSettings (outputFormat))
import qualified Network.Wai.Middleware.RequestLogger      as Logger
import qualified Network.Wai.Middleware.RequestLogger.JSON as Logger
import           TonaParser                                (Parser, (.||))
import qualified TonaParser
import           Tonatona                                  (HasConfig (..), HasParser (..), config)
import qualified Tonatona.Logger                           as TonaLogger
import           Web.Scotty                                (ScottyM, middleware, scotty)

run
  :: forall env. (HasConfig env Config, HasConfig env TonaLogger.Config)
  => ScottyM ()
  -> RIO env ()
run scottyServer = do
  env <- ask
  conf <- asks config :: RIO env Config
  loggingMiddleware <- mkLoggerMiddlware
  liftIO $ scotty (port conf) $ do
    middleware loggingMiddleware
    scottyServer

mkLoggerMiddlware :: (HasConfig env TonaLogger.Config) => RIO env Middleware
mkLoggerMiddlware = do
  TonaLogger.Config {mode, verbose} <- asks config
  case (mode, verbose) of
    (TonaLogger.Development, TonaLogger.Verbose True) ->
      liftIO mkLogRequestVerbose

    (TonaLogger.Development, TonaLogger.Verbose False) ->
      pure Logger.logStdoutDev

    (_, TonaLogger.Verbose True) ->
      pure Logger.logStdoutDev

    (_, TonaLogger.Verbose False) ->
      pure Logger.logStdout

mkLogRequestVerbose :: IO Middleware
mkLogRequestVerbose = do
  Logger.mkRequestLogger def
    { outputFormat =
        Logger.CustomOutputFormatWithDetailsAndHeaders
          Logger.formatAsJSONWithHeaders
    }

newtype Config =
  Config
    { port :: Port
    }
    deriving (Show)

portParser :: Parser Port
portParser =
  TonaParser.optionalVal
    "Port to serve"
    (TonaParser.argLong "port" .|| TonaParser.envVar "PORT")
    8000

instance HasParser Config where
  parser =
    Config <$> portParser
