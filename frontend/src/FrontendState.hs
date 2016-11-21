{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module FrontendState where

------------------------------------------------------------------------------
import           Control.Concurrent
import qualified Control.Concurrent.Thread.Delay as Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils
import           Reflex.Dom.SemanticUI.Modal
------------------------------------------------------------------------------

data FrontendState t = FrontendState
    { fsMyName        :: Dynamic t (Maybe User)
    }

stateManager
    :: MonadWidget t m
    => m (FrontendState t)
stateManager go = do
    pb <- getPostBuild
    startTime <- performEvent (liftIO getCurrentTime <$ pb)
    timerEvent <- myTickLossyFrom 1 startTime

    let upEvent = mergeWith (++) $ map (fmap (:[]))
          [ Up_SendChatMessage <$> goSendMessage go
          ]
    (downEvent, _) <- openWebSocket $ traceEvent "upEvent" upEvent
    myName <- holdDyn Nothing $ fmap Just $ fmapMaybe (isMsg _Down_AuthenticatedUser) downEvent
    let msgEvent = fmapMaybe (isMsg _Down_Message) downEvent
    modalData <- holdDyn (InfoMsg, "This is a dummy message") msgEvent
    modalMarkup (ShowModal <$ msgEvent) modalData
    return $ FrontendState myName

modalMarkup
    :: MonadWidget t m
    => Event t ModalBehavior
    -> Dynamic t (MessageType, Text)
    -> m ()
modalMarkup e msgPair = do
    uiModal e $ do
      divClass "header" $ dynText $ mkHeader . fst <$> msgPair
      divClass "content" $ el "p" $ dynText $ snd <$> msgPair
      divClass "actions" $ divClass "ui approve button" $ text "OK"
  where
    mkHeader ErrorMsg = "Error"
    mkHeader InfoMsg = "Info"

isMsg :: Prism' Down a -> Maybe Down -> Maybe a
isMsg constructorPrism md = (^? constructorPrism) =<< md

gateDyn :: Reflex t => Dynamic t Bool -> Event t a -> Event t a
gateDyn d e = attachPromptlyDynWithMaybe (\b a -> if b then Just a else Nothing) d e

------------------------------------------------------------------------------
openWebSocket :: MonadWidget t m => Event t [Up] -> m (Event t (Maybe Down), Event t ())
openWebSocket wsUp = do
    wv <- askWebView
    host :: Text <- liftIO $ getLocationHost wv
    protocol :: Text <- liftIO $ getLocationProtocol wv
    let wsProtocol = case protocol of
          "" -> "ws:" -- We're in GHC
          "about:" -> "ws:" -- We're in GHC
          "file:" -> "ws:"
          "http:" -> "ws:"
          "https:" -> "wss:"
          _ -> error $ "Unrecognized protocol: " <> show protocol
        wsHost = case protocol of
          "" -> "localhost:8000" -- We're in GHC
          "about:" -> "localhost:8000" -- We're in GHC
          "file:" -> "localhost:8000"
          _ -> host
    rec ws <- webSocket (wsProtocol <> "//" <> wsHost <> "/websocket") $
          def & webSocketConfig_send .~ send
        websocketReady <- holdDyn False $ True <$ _webSocket_open ws
        buffer <- foldDyn (++) [] $ gateDyn (not <$> websocketReady) wsUp
        let send = (fmap (LBS.toStrict . encode)) <$> leftmost
                     [ gateDyn websocketReady wsUp
                     , tag (current buffer) (_webSocket_open ws)
                     ]
    let rawReceived = _webSocket_recv ws
    return $ (decode' . LBS.fromStrict <$> rawReceived, _webSocket_open ws)
