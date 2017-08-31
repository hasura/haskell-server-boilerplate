{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Hasura.HTTP.Server
  ( ErrResp (..)
  , mkErrResp
  , ToErrResp (..)

  , HandlerT(..)
  , Handler
  , handlerToSpock
  , runSpockServer

  , tryIOWith
  , liftEither
  , unwrapMaybe
  , decodeStrictWith
  , exitOnError
  ) where

import           Control.Exception         (IOException, try)
import           Control.Monad.Except      (ExceptT (..), MonadError,
                                            runExceptT, throwError)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Logger      (LoggingT, MonadLogger,
                                            MonadLoggerIO, logInfo, runLoggingT)
import           Control.Monad.Reader      (MonadReader, ReaderT, runReaderT)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Aeson.Text           (encodeToTextBuilder)
import           Data.Bifunctor            (first)
import           Data.Bool                 (bool)
import           Data.Monoid               ((<>))
import           Data.Text.Lazy            (toStrict)
import           Data.Text.Lazy.Builder    (toLazyText)
import           Data.Time                 (UTCTime, defaultTimeLocale,
                                            formatTime, getCurrentTime)
import           System.Exit               (exitFailure)

import qualified Control.Monad.Logger      as L
import qualified Data.Aeson                as J
import qualified Data.Aeson.Casing         as JC
import qualified Data.Aeson.TH             as JT
import qualified Data.ByteString           as B
import qualified Data.Text                 as T
import qualified Network.HTTP.Types.Status as N
import qualified System.Log.FastLogger     as FL
import qualified Web.Spock.Core            as S

data ErrResp
  = ErrResp
  { _erCode    :: !T.Text
  , _erMessage :: !T.Text
  , _erDetail  :: !(Maybe J.Value)
  } deriving (Show, Eq)

$(JT.deriveJSON (JC.aesonDrop 3 JC.camelCase) ''ErrResp)

mkErrResp :: T.Text -> T.Text -> ErrResp
mkErrResp c m = ErrResp c m Nothing

class ToErrResp e where
  toErrResp :: e -> (N.Status, ErrResp)

newtype HandlerT e r m a =
  HandlerT { runHandlerT :: ExceptT e (ReaderT r (LoggingT m)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader r
           , MonadError e
           , MonadLogger
           , MonadIO
           , MonadLoggerIO
           )

instance MonadTrans (HandlerT e r) where
  lift action = HandlerT $ lift $ lift $ lift action

-- simpler handler
type Handler e r = HandlerT e r IO

handlerToSpock
  :: ( J.ToJSON a
     , ToErrResp e
     , MonadIO m )
  => r
  -> HandlerT e r m a
  -> S.ActionT (LoggingT m) ()
handlerToSpock r handler = do
  res <- lift $ flip runReaderT r $ runExceptT $ runHandlerT handler
  (st, resp) <- either onError onSuccess res
  S.setStatus st
  S.json resp
  where
    onSuccess a = return (N.status200, J.toJSON a)
    onError e = do
      lift $ $(logInfo) $ encToTxt $ snd $ toErrResp e
      return $ fmap J.toJSON $ toErrResp e

mkServerLog
  :: UTCTime
  -> L.Loc
  -> L.LogSource
  -> L.LogLevel
  -> FL.LogStr
  -> FL.LogStr
mkServerLog logTime loc src level msg =
  mconcat
  [ "[" <> FL.toLogStr timestamp <> "] "
  , "["
  , defaultLogLevelStr
  , bool ("#" <> FL.toLogStr src) mempty $ T.null src
  , "] "
  , "[" <> FL.toLogStr fileLocStr <> "] "
  , msg
  , "\n"
  ]
  where
    timestamp = formatTime defaultTimeLocale "%FT%T%QZ" logTime
    line = fst $ L.loc_start loc
    fileLocStr = L.loc_filename loc ++ ':' : show line

    defaultLogLevelStr = case level of
      L.LevelOther t -> FL.toLogStr t
      _              -> FL.toLogStr $ drop 5 $ show level

runSpockServer
  :: Int
  -> T.Text
  -> (N.Status -> (N.Status, ErrResp))
  -> S.SpockT (LoggingT IO) () -> IO ()
runSpockServer serverPort serverName globalErrHandler action = do
  stdoutLS <- FL.newStdoutLoggerSet FL.defaultBufSize
  -- Start the server
  S.runSpockNoBanner serverPort $
    S.spockConfigT spockConfig (flip runLoggingT (logger stdoutLS)) $ do
    lift $ $(logInfo) $ "starting " <> serverName <> " server"
    action

  where
    logger loggerSet l logSrc logLevel logStr = do
      curTime <- getCurrentTime
      FL.pushLogStr loggerSet $
        mkServerLog curTime l logSrc logLevel logStr

    -- Spock server options
    spockConfig = S.SpockConfig Nothing fallbackErrHandler

    -- For 404 and uncaught exceptions
    fallbackErrHandler st = do
      let (respSt, respMsg) = globalErrHandler st
      S.setStatus respSt
      S.json respMsg

-- Helper functions

tryIOWith
  :: ( MonadError e m
     , MonadIO m )
  => (IOException -> e)
  -> IO a
  -> m a
tryIOWith ef action = do
  res <- liftIO $ try action
  either (throwError . ef) return res

liftEither :: (MonadError e m) => Either e a -> m a
liftEither = either throwError return

decodeStrictWith
  :: ( J.FromJSON a
     , MonadError e m)
  => (String -> e)
  -> B.ByteString
  -> m a
decodeStrictWith ef  =
  liftEither . first ef . J.eitherDecodeStrict

unwrapMaybe :: (MonadError e m) => m a -> Maybe a -> m a
unwrapMaybe e = maybe e return

encToTxt :: (J.ToJSON a) => a -> T.Text
encToTxt = toStrict . toLazyText . encodeToTextBuilder

exitOnError :: ExceptT String IO a -> IO a
exitOnError action = do
  res <- runExceptT action
  either (\e -> putStrLn e >> exitFailure) return res
