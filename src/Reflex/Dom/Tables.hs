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

module Reflex.Dom.Tables where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Writer.Strict (MonadWriter(..), execWriter)
import Data.Default
import Data.Foldable.WithIndex
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Semigroup (Any(..), All(..))
import Data.Traversable
import GHC.Generics ((:*:)(..))
import HigherKinded
import HigherKinded.HKD.Base
import Reflex.Dom hiding (Attrs, El)
import Reflex.Dom.Attrs

import Reflex.Dom.Tables.Internal



data TableConfig key row th td t m = TableConfig
  { tableConfig_columns :: [TableColumn key row (m th) (m td)] -- ^ columns configuration
  , tableConfig_tableAttrs :: [Attrs t m] -- ^ <table> attributes
  , tableConfig_theadAttrs :: [Attrs t m] -- ^ <thead> attributes
  , tableConfig_tbodyAttrs :: [Attrs t m] -- ^ <tbody> attributes
  , tableConfig_thAttrs :: Maybe th -> [Attrs t m] -- ^ <th> attributes
  , tableConfig_trAttrs :: key -> row -> [Attrs t m] -- ^ <tr> attributes
  , tableConfig_tdAttrs :: td -> key -> row -> [Attrs t m] -- ^ <td> attributes
  }

instance Reflex t => Default (TableConfig key row th td t m) where
  def = TableConfig
    { tableConfig_columns = def
    , tableConfig_tableAttrs = def
    , tableConfig_theadAttrs = def
    , tableConfig_tbodyAttrs = def
    , tableConfig_thAttrs = \_ -> def
    , tableConfig_trAttrs = \_ _ -> def
    , tableConfig_tdAttrs = \_ _ _ -> def
    }



data TableColumn key row th td
  = TD (key -> row -> td)
  | TH (th, key -> row -> td)
  | THs (th, [TableColumn key row th td])

getTDs :: [TableColumn key row th td] -> [key -> row -> td]
getTDs tableCols = go tableCols []
  where
    go [] !tds = tds
    go (TD td : cols) !tds = td : go cols tds
    go (TH (_, td) : cols) !tds = td : go cols tds
    go (THs (_, thcols) : cols) !tds = go thcols $ go cols tds

getTHRows :: [TableColumn key row th td] -> [[Maybe (th, Int)]]
getTHRows tableCols = takeWhile (\ths -> (not $ null ths) && (not $ all (isNothing) ths)) $ fmap (go tableCols) [0 :: Int ..]
  where
    go [] _ = []
    go (TD _ : cols) 0 = go cols 0
    go (TH (h, _) : cols) 0 = Just (h, 1) : go cols 0
    go (THs (h, thcols) : cols) 0 = Just (h, length thcols) : go cols 0
    go (TD _ : cols) i = Nothing : go cols i
    go (TH (_, _) : cols) i = Nothing : go cols i
    go (THs (_, thcols) : cols) i = go thcols (i - 1) <> go cols i

mapTableColumn
  :: ( (th -> th')
     , ((key -> row -> td) -> (key' -> row' -> td'))
     )
  -> TableColumn key row th td
  -> TableColumn key' row' th' td'
mapTableColumn (thF, tdF) tableColumn = go tableColumn
  where
    go (TD td) = TD $ tdF td
    go (TH (th, td)) = TH (thF th, tdF td)
    go (THs (th, thcols)) = THs (thF th, go <$> thcols)

forTableColumn
  :: TableColumn key row th td
  -> ( (th -> th')
     , ((key -> row -> td) -> (key' -> row' -> td'))
     )
  -> TableColumn key' row' th' td'
forTableColumn = flip mapTableColumn



data Table key row th td t m = Table
  { table_tableEl :: El t m
  , table_theadEl :: El t m
  , table_tbodyEl :: El t m
  , table_thEls :: [[El t m]]
  , table_trEls :: Map key (El t m)
  , table_tdEls :: Map key [El t m]
  , table_thVals :: [[th]]
  , table_trVals :: Map key [td]
  , table_ths :: [[(El t m, th)]]
  , table_trs :: Map key (El t m, [(El t m, td)])
  }

tableDyn
  :: forall key row th td t m.
     ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Ord key
     , Eq row
     )
  => Dynamic t (Map key row)
  -> TableConfig key (Dynamic t row) th td t m
  -> m (Dynamic t (Table key row th td t m))
tableDyn rows cfg = do
  let columns = tableConfig_columns cfg
      headerRows = getTHRows columns
      cols = getTDs columns
      tableAttrs = tableConfig_tableAttrs cfg
      theadAttrs = tableConfig_theadAttrs cfg
      tbodyAttrs = tableConfig_tbodyAttrs cfg
      thAttrs = tableConfig_thAttrs cfg
      trAttrs = tableConfig_trAttrs cfg
      tdAttrs = tableConfig_tdAttrs cfg

  (tableEl, ((theadEl, ths), (tbodyEl, trsDyn))) <- elAttrs' "table" tableAttrs $ do

    thead :: (El t m, [[(El t m, th)]]) <-
      elAttrs' "thead" theadAttrs $
        for headerRows $ \headers ->
          el "tr" $
            fmap catMaybes $ for headers $ \case
              Nothing -> do
                void $ elAttrs' "th" (thAttrs Nothing) $ blank
                pure Nothing
              Just (header, colspan) -> mdo
                th@(_, thVal) <- elAttrs' "th"
                  ( thAttrs (Just thVal)
                  <> case colspan of
                      1 -> []
                      _ -> ["colspan" ~: show colspan]
                  ) header
                pure $ Just th

    tbody :: (El t m, Dynamic t (Map key (El t m, [(El t m, td)]))) <-
      elAttrs' "tbody" tbodyAttrs $
        listWithKey rows $ \k row ->
          elAttrs' "tr" (trAttrs k row) $
            for cols $ \col -> mdo
              td@(_, tdVal) <- elAttrs' "td" (tdAttrs tdVal k row) $ col k row
              pure td

    pure (thead, tbody)

  pure $ ffor trsDyn $ \trs ->
    Table
      { table_tableEl = tableEl
      , table_theadEl = theadEl
      , table_tbodyEl = tbodyEl
      , table_thEls = fmap fst <$> ths
      , table_trEls = fst <$> trs
      , table_tdEls = ffor trs $ fmap fst . snd
      , table_thVals = fmap snd <$> ths
      , table_trVals = ffor trs $ fmap snd . snd
      , table_ths = ths
      , table_trs = trs
      }

tableDynView
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , Ord key
     , Eq row
     )
  => Dynamic t (Map key row)
  -> TableConfig key (Dynamic t row) th (Event t td) t m
  -> m (Event t (Table key row th td t m))
tableDynView rows cfg = do
  tableD <- tableDyn rows cfg
  let tableDEv = ffor tableD $ \table ->
        let trsEv = mergeMap $ ffor (table_trs table) $ \(trEl, tds) ->
              fmap ((trEl,) . NE.toList) $ mergeList $ fmap (\(tdEl, td) -> (tdEl,) <$> td) tds
        in ffor trsEv $ \trs ->
          Table
            { table_tableEl = table_tableEl table
            , table_theadEl = table_theadEl table
            , table_tbodyEl = table_tbodyEl table
            , table_thEls = table_thEls table
            , table_trEls = table_trEls table
            , table_tdEls = table_tdEls table
            , table_thVals = table_thVals table
            , table_trVals = ffor trs $ fmap snd . snd
            , table_ths = table_ths table
            , table_trs = trs
            }
  pure $ switchDyn tableDEv

tableStaticView
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , FoldableWithIndex key f
     , Ord key
     , Eq row
     )
  => f row
  -> TableConfig key row th (Event t td) t m
  -> m (Event t (Table key row th td t m))
tableStaticView rows' cfg' = do
  let rows = constDyn $ Map.fromList $ itoList rows'
      cfg = TableConfig
        { tableConfig_tableAttrs = tableConfig_tableAttrs cfg'
        , tableConfig_theadAttrs = tableConfig_theadAttrs cfg'
        , tableConfig_tbodyAttrs = tableConfig_tbodyAttrs cfg'
        , tableConfig_thAttrs = tableConfig_thAttrs cfg'
        , tableConfig_trAttrs = \key rowDyn ->
            [ foldDynAttrs $ (tableConfig_trAttrs cfg') key <$> rowDyn ]
        , tableConfig_tdAttrs = \td key rowDyn ->
            [ foldDynAttrs $ (tableConfig_tdAttrs cfg') td key <$> rowDyn ]
        , tableConfig_columns = ffor (tableConfig_columns cfg') $ mapTableColumn $
            ( id
            , \col -> \k rowDyn -> do
                row <- sample $ current rowDyn
                col k row
            )
        }
  tableDynView rows cfg

tableStatic
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , FoldableWithIndex key f
     , Ord key
     , Eq row
     )
  => f row
  -> TableConfig key row th td t m
  -> m (Table key row th td t m)
tableStatic rows' cfg' = do
  let rows = constDyn $ Map.fromList $ itoList rows'
      cfg = TableConfig
        { tableConfig_tableAttrs = tableConfig_tableAttrs cfg'
        , tableConfig_theadAttrs = tableConfig_theadAttrs cfg'
        , tableConfig_tbodyAttrs = tableConfig_tbodyAttrs cfg'
        , tableConfig_thAttrs = tableConfig_thAttrs cfg'
        , tableConfig_trAttrs = \key rowDyn ->
            [ foldDynAttrs $ (tableConfig_trAttrs cfg') key <$> rowDyn ]
        , tableConfig_tdAttrs = \td key rowDyn ->
            [ foldDynAttrs $ (tableConfig_tdAttrs cfg') td key <$> rowDyn ]
        , tableConfig_columns = ffor (tableConfig_columns cfg') $ mapTableColumn $
            ( id
            , \col -> \k rowDyn -> do
                row <- sample $ current rowDyn
                col k row
            )
        }
  table <- tableDyn rows cfg
  sample $ current table



type TableSortConfig columns = columns |> TableSortConfigF
type TableSortConfigF = Const (Maybe Bool)

tableSortedRows
  :: forall key row t m.
     ( MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , PerformEvent t m
     --
     , ConstructHKD (F row) row Applied Identity
     , HKDFieldsHave Ord (F row)
     , IsHKD (F row) Applied Identity
     , IsHKD (F row) Applied (Identity :*: Identity)
     , IsHKD (F row) Applied (Dict Ord)
     , IsHKD (F row) Applied (Dict Ord `Product` (Identity :*: Identity))
     , IsHKD (F row) Applied TableSortConfigF
     )
  => Dynamic t (Map key row)
  -> m
      ( Dynamic t (Map Int (key, row))
      , ( Dynamic t (TableSortConfig row)
        , (TableSortConfig row -> TableSortConfig row) -> IO ()
        )
      )
tableSortedRows rowsDyn = do
  (triggerSortEv, triggerSort) <- newTriggerEvent

  sortConfigDyn <- foldDyn ($) (pureF $ Const Nothing) triggerSortEv

  let sortedRowsDyn = tableSortRows <$> sortConfigDyn <*> rowsDyn

  pure
    ( sortedRowsDyn
    , ( sortConfigDyn
      , triggerSort
      )
    )

tableSortRows
  :: forall key row.
     ( ConstructHKD (F row) row Applied Identity
     , HKDFieldsHave Ord (F row)
     , IsHKD (F row) Applied Identity
     , IsHKD (F row) Applied (Identity :*: Identity)
     , IsHKD (F row) Applied (Dict Ord)
     , IsHKD (F row) Applied (Dict Ord `Product` (Identity :*: Identity))
     , IsHKD (F row) Applied TableSortConfigF
     )
  => TableSortConfig row
  -> Map key row
  -> Map Int (key, row)
tableSortRows sortConfig rowsMap = Map.fromAscList $ zip [0..] sortedRowsList
  where
    sortedRowsList = case isSortActive of
      True -> List.sortBy sorter rowsList
      False -> rowsList
    --
    isSortActive =
      or $ execWriter $ traverseF
        ( \columnConfig -> do
            case columnConfig of
              Const (Just _) -> tell [True]
              _ -> pure ()
            pure columnConfig
        )
        sortConfig
    --
    sorter (_, a) (_, b) = mconcat orderings
      where
        orderings =
          execWriter $ bitraverseF
            ( \columnConfig (Pair Dict (colA :*: colB)) -> do
                case columnConfig of
                  Const (Just direction) ->
                    case direction of
                      True -> tell $ [compare colA colB]
                      False -> tell $ [compare colB colA]
                  _ -> pure ()
                pure columnConfig
            )
            sortConfig
            zippedAB
        --
        zippedAB =
            withConstrainedFieldsHKD @Ord @(F row) @Applied
          $ zipF (:*:) hkA hkB
        --
        hkA = F (Identity a)
        hkB = F (Identity b)
    --
    rowsList = Map.toList rowsMap



type TableFilterConfig key columns = columns |> TableFilterConfigF key columns
type TableFilterConfigF key columns = Maybe `Compose` TableFilterPredicate key columns

data TableFilterPredicate key columns a
  = TableFilterPredicate_Any (key -> columns -> a -> Bool)
  | TableFilterPredicate_All (key -> columns -> a -> Bool)

tableFilteredRows
  :: forall key row t m.
     ( MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , PerformEvent t m
     --
     , ConstructHKD (F row) row Applied Identity
     , IsHKD (F row) Applied Identity
     , IsHKD (F row) Applied (TableFilterConfigF key row)
     )
  => Dynamic t (Map key row)
  -> m
      ( Dynamic t (Map key row)
      , ( Dynamic t (TableFilterConfig key row)
        , (TableFilterConfig key row -> TableFilterConfig key row) -> IO ()
        )
      )
tableFilteredRows rowsDyn = do
  (triggerFilterEv, triggerFilter) <- newTriggerEvent

  filterConfigDyn <- foldDyn ($) (pureF $ Compose Nothing) triggerFilterEv

  let filteredRowsDyn = tableFilterRows <$> filterConfigDyn <*> rowsDyn

  pure
    ( filteredRowsDyn
    , ( filterConfigDyn
      , triggerFilter
      )
    )

tableFilterRows
  :: forall key row.
     ( ConstructHKD (F row) row Applied Identity
     , IsHKD (F row) Applied Identity
     , IsHKD (F row) Applied (TableFilterConfigF key row)
     )
  => TableFilterConfig key row
  -> Map key row
  -> Map key row
tableFilterRows filterConfig rowsMap = filteredRowsMap
  where
    filteredRowsMap = case isFilterActive of
      True -> Map.filterWithKey filterer rowsMap
      False -> rowsMap
    --
    isFilterActive =
      getAny $ execWriter $ traverseF
        ( \columnConfig -> do
            case columnConfig of
              Compose (Just _) -> tell $ Any True
              _ -> pure ()
            pure columnConfig
        )
        filterConfig
    --
    filterer key x =
        case (getAny <$> anyM, getAll <$> allM) of
          (Just anys, Just alls) -> anys && alls
          (Just anys, _) -> anys
          (_, Just alls) -> alls
          _ -> True
      where
        (anyM, allM) = execWriter $ bitraverseF
          ( \columnConfig (Identity colX) -> do
              case columnConfig of
                Compose (Just (TableFilterPredicate_Any f)) -> tell (Just $ Any $ f key x colX, Nothing)
                Compose (Just (TableFilterPredicate_All f)) -> tell (Nothing, Just $ All $ f key x colX)
                _ -> pure ()
              pure (Identity colX)
          )
          filterConfig
          hkX
        hkX = F (Identity x)

