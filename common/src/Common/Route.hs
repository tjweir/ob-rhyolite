{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Obelisk.Route
import Obelisk.Route.TH (deriveRouteComponent)

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- | Websocket listen
  BackendRoute_Listen :: BackendRoute ()

data FrontendRoute :: * -> * where
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.
  FrontendRoute_Main :: FrontendRoute ()

fullRouteEncoder ::
  Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder =
  mkFullRouteEncoder
    (FullRoute_Backend BackendRoute_Missing :/ ())
    ( \case
        BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
        BackendRoute_Listen -> PathSegment "listen" $ unitEncoder mempty
    )
    ( \case
        FrontendRoute_Main -> PathEnd $ unitEncoder mempty
    )

checkedFullRouteEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = case checkEncoder fullRouteEncoder of
  Left e -> error (show e)
  Right x -> x

concat
  <$> traverse
    deriveRouteComponent
    [ ''BackendRoute,
      ''FrontendRoute
    ]
