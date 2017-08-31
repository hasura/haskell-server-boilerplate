{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad.Except
import           Control.Monad.Logger
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Hasura.HTTP.Server         as SB
import           Hasura.Notify.Context
import           Hasura.Notify.Handlers
import           Hasura.Notify.Types
import           System.Environment         (lookupEnv)
import           Web.Spock.Core
import           Data.Text

data MyErrorType = undefined
data MyConf = undefined

type MyContext = SB.Handler MyErrorType MyConf

apiServer :: SpockT (LoggingT IO) ()
apiServer = do
  get root $ do
    someValue <- header "X-Some-Header"
    SB.handlerToSpock $ do
      someConfig <- ask
      -- FIXME: something

fallbackErrHandler :: HT.Status -> (HT.Status, SB.ErrResp)
fallbackErrHandler = undefined

main :: IO ()
main = do
  SB.runSpockServer 8080 "sample" fallbackErrHandler apiServer

