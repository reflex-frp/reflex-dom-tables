{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Dom.Tables.Internal where

import Control.Monad.Fix
import Data.Functor.Identity
import Data.Map (Map)
import HigherKinded
import Reflex.Dom hiding (Attrs, El)



type El t m = Element EventResult (DomBuilderSpace m) t


withHKDRows
  :: forall hkt f row rowHKD.
     ( Functor f
     , ConstructHKD rowHKD row hkt Identity
     )
  => (f (rowHKD Identity) -> f (rowHKD Identity))
  -> f row
  -> f row
withHKDRows f rows = resultRows
  where
    resultRows :: f row
    resultRows = runIdentity . fromHKD @rowHKD @row @hkt <$> resultRows'
    --
    resultRows' :: f (rowHKD Identity)
    resultRows' = f rows'
    --
    rows' :: f (rowHKD Identity)
    rows' = toHKD @rowHKD @row @hkt . Identity <$> rows


withHKDIndexedRows
  :: forall hkt rowHKD key row.
     ( ConstructHKD rowHKD row hkt Identity
     )
  => (Map key (rowHKD Identity) -> Map Int (key, (rowHKD Identity)))
  -> Map key row
  -> Map Int (key, row)
withHKDIndexedRows f rows = resultRows
  where
    resultRows :: Map Int (key, row)
    resultRows = (\(key, row) -> (key, runIdentity $ fromHKD @rowHKD @row @hkt row)) <$> resultRows'
    --
    resultRows' :: Map Int (key, rowHKD Identity)
    resultRows' = f rows'
    --
    rows' :: Map key (rowHKD Identity)
    rows' = toHKD @rowHKD @row @hkt . Identity <$> rows


foldDynWithTrigger
  :: forall x y x' t m.
     ( MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , PerformEvent t m
     )
  => (Dynamic t y -> Dynamic t x -> Dynamic t x')
  -> y
  -> Dynamic t x
  -> m
      ( Dynamic t x'
      , ( Dynamic t y
        , (y -> y) -> IO ()
        )
      )
foldDynWithTrigger f xInit xDyn = do
  (triggerEv, trigger)
    :: ( Event t (y -> y)
       , (y -> y) -> IO ()
       )
    <- newTriggerEvent

  yDyn
    :: Dynamic t y
    <- foldDyn ($) xInit triggerEv

  let xDyn' :: Dynamic t x'
      xDyn' = f yDyn xDyn

  pure
    ( xDyn'
    , ( yDyn
      , trigger
      )
    )
