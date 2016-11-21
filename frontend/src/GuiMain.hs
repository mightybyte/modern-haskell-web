{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

------------------------------------------------------------------------------
import           Control.Lens           hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Bool (bool)
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import qualified GHCJS.DOM.Element as DOM
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils
------------------------------------------------------------------------------
import           WsApi
------------------------------------------------------------------------------


menu :: MonadWidget t m => Dynamic t (Maybe Player) -> m ()
menu myName = do
    divClass "ui blue inverted attached borderless menu" $ do
      elClass "span" "item active" $ text "Modern Haskell Web"
      divClass "right menu" $ do
        elAttr "a" ("class" =: "item" <> "href" =: "/logout") $
          dynText $ signOut <$> myName
    return ()
  where
    signOut Nothing = "Sign Out"
    signOut (Just nm) = T.unwords ["Sign Out", _playerName nm]

runApp :: MonadWidget t m => m ()
runApp = do
    pb <- getPostBuild
    resp <- performRequestAsync $
      postJson "/cards" (object [t"foo" .= t"bar"]) <$ pb
    let cacheE = decodeXhrResponse <$> resp
    cardCache <- liftM uniqDyn $ holdDyn mempty $ fmap jsonToCache cacheE

    divClass "full-height" $ do
      rec menu myName
          myName <- elAttr "div" ("class" =: "ui two column padded stackable grid" <>
                        "style" =: "height: calc(100% - 40px)") $ do

            rec fs <- stateManager
            return $ fsMyName fs
      return ()

------------------------------------------------------------------------------
main :: IO ()
main = mainWidget runApp
