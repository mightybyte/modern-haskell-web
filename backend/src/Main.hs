{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Error
import qualified Control.Exception    as E
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Configurator    as C
import           Data.IORef
import           Data.List     hiding (insert)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Map.Syntax ((##))
import           Data.Maybe
import           Data.Monoid
import           Data.Ord             (comparing)
import           Data.Pool            (Pool)
import           Data.RNG
import           Data.Readable
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.String.Conv
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8)
import qualified Data.Text.Lazy       as LazyText
import           Data.Time
import           Data.Time.Clock.POSIX
import           Database.Groundhog
import           Database.Groundhog.Core
import           Database.Groundhog.Postgresql
import qualified Heist.Interpreted as I
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import           Snap
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Session (SessionManager)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Util.FileServe (serveDirectory)
import qualified Database.Groundhog.Postgresql as GP
------------------------------------------------------------------------------
import           GroundhogAuth
------------------------------------------------------------------------------

data App = App
    { _appHeist       :: Snaplet (Heist App)
    , _appSess        :: Snaplet SessionManager
    , _appAuth        :: Snaplet (AuthManager App)
    , _appDb          :: Pool GP.Postgresql
    , _appConns       :: WS.Connection
    }

makeLenses ''App

routes :: [(ByteString, Handler App App ())]
routes = [ ("static", serveDirectory "static")
         , ("login"   , with appAuth handleLoginSubmit)
         , ("new_user", with appAuth handleNewUser)
         , ("logout"  , with appAuth handleLogout)

           -- Just testing
         -- , ("tellServer", tellHandler)
         , ("websocket", handleApi)
         ]


handleApi :: Handler App App ()
handleApi = do
  mu <- with appAuth currentUser
  case mu of
    Nothing -> return ()
    Just u -> do
      (ConnRepo cRepo) <- asks appConns
      app <- ask
      WS.runWebSocketsSnap $ \pendingConn -> do
        conn <- WS.acceptRequest pendingConn
        WS.forkPingThread conn 10
        conns <- atomicModifyIORef cRepo (insertAndReturn u conn)

        forM_ conns $ \c -> do
          wsSend c (Down_ListPlayers (M.keys conns))

        talkClient app conn (u)

wsReceive :: FromJSON a => WS.Connection -> IO (Either String a)
wsReceive conn = do
    dm <- WS.receiveDataMessage conn
    return $ eitherDecode' $ dataToBs dm

wsSend :: ToJSON a => WS.Connection -> a -> IO ()
wsSend conn v = WS.sendTextData conn $ encode v

dataToBs :: WS.DataMessage -> BSL.ByteString
dataToBs (WS.Text bs) = bs
dataToBs (WS.Binary bs) = bs


appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Template App" Nothing $ do
    addRoutes routes
    rng <- liftIO mkRNG
    ratings <- liftIO $ newIORef mempty
    appCardsJ <- buildCardsJson rng

    cfg <- C.subconfig "postgresql" <$> getSnapletUserConfig
    connstr <- liftIO $ decodeUtf8  <$> getConnectionString cfg
    p <- liftIO $ GP.withPostgresqlPool (T.unpack connstr) 3 return
    c <- liftIO $ extractConn return p
    liftIO $ runNoLoggingT (withConn (runDbPersist migrateDB) c)

    h   <- nestSnaplet "" appHeist $ heistInit ""
    s   <- nestSnaplet "sess" appSess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
    a   <- nestSnaplet "auth" appAuth $
             -- initJsonFileAuthManager defAuthSettings sess "site_users.json"
             initGroundhogAuth appSess p
    addAuthSplices h appAuth

    return $ App h s a p

migrateDB :: (MonadIO m, GP.PersistBackend m) => m ()
migrateDB = runMigration $ do
    --GP.migrate (undefined :: CurrentRating)
    return ()

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice e = "loginError" ## I.textSplice e


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing (\_ -> handleLogin e) (redirect "/")
  where
    e = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

instance HasHeist App where
  heistLens = subSnaplet appHeist

main :: IO ()
main = do (_, appHandler , _) <- runSnaplet Nothing appInit
          quickHttpServe appHandler

gh :: DbPersist GP.Postgresql (NoLoggingT IO) a -> Handler App App a
gh f = do
  cp <- reader _appDb
  liftIO $ extractConn return cp >>=
    runNoLoggingT . (withConn (runDbPersist f))
