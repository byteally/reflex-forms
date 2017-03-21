{-# LANGUAGE DeriveGeneric, DefaultSignatures, FlexibleContexts, TypeOperators, KindSignatures, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, GADTs, RecursiveDo, OverloadedStrings #-}
module Reflex.Forms
  ( ToWidget (..)
  )where

import Reflex.Dom
import Reflex
import GHC.Generics
import Data.Proxy
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Typeable
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as ASCII
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import           Data.Time.Format                   (FormatTime,
                                                     defaultTimeLocale,
                                                     formatTime, parseTimeM)
import Text.Read


class ToWidget a where
  toWidget :: MonadWidget t m => Maybe a -> m (Dynamic t (Maybe a))
  default toWidget :: (MonadWidget t m, Generic a, GToWidget (Rep a)) => Maybe a -> m (Dynamic t (Maybe a))
  toWidget wDef = mapDyn (fmap to) =<< gToWidget (GToWidgetOpts Nothing (fmap from wDef) Nothing)

instance ToWidget Text where
  toWidget wDef = do
    let def' = def
                & textInputConfig_inputType .~ "text"
                & attributes .~ constDyn ("class" =: "text-box")
        textDef = case wDef of
          Nothing -> def'
          Just a -> def' & textInputConfig_initialValue .~ a
    txt <- textInput textDef
    mapDyn Just $ _textInput_value txt

instance ToWidget Int where
  toWidget wDef = do
    let def' = def
                & textInputConfig_inputType .~ "number"
                & attributes .~ constDyn ("class" =: "text-box")
        intDef = case wDef of
          Nothing -> def'
          Just a -> def' & textInputConfig_initialValue .~ (T.pack $ show a)
    txt <- textInput intDef
    mapDyn (readMaybe . T.unpack) $ _textInput_value txt

instance ToWidget Double where
  toWidget wDef = do
    let def' = def
                & textInputConfig_inputType .~ "number"
                & attributes .~ constDyn ("class" =: "text-box")
        doubleDef = case wDef of
          Nothing -> def'
          Just a -> def' & textInputConfig_initialValue .~ (T.pack $ show a)
    txt <- textInput doubleDef
    mapDyn (readMaybe . T.unpack) $ _textInput_value txt

instance ToWidget Bool where
  toWidget wDef = do
    chk <- checkbox (fromMaybe False wDef) def
    mapDyn Just $ _checkbox_value chk

instance ToWidget () where
  toWidget _ = return $ constDyn $ Just ()


instance ToWidget UTCTime where
  toWidget wDef = do
    let def' = def
                & attributes .~ constDyn ("class" =: "text-box")
        utcDef = maybe def' (\a -> def' & textInputConfig_initialValue .~ (T.pack $ formatUTC a)) wDef
    txt <- textInput utcDef
    mapDyn (parseUTC . T.unpack) $ _textInput_value txt

parseUTC :: String -> Maybe UTCTime
parseUTC str = parseTimeM True defaultTimeLocale "%FT%T%QZ" str

parseLocalTime :: String -> Maybe LocalTime
parseLocalTime str = parseTimeM True defaultTimeLocale "%FT%T" str

parseDay :: String -> Maybe Day
parseDay str = readMaybe str

parseTimeOfDay :: String -> Maybe TimeOfDay
parseTimeOfDay str = parseTimeM True defaultTimeLocale "%T" str

formatUTC :: UTCTime -> String
formatUTC utc =
  let format = "%FT%T." ++ formatSubseconds utc ++ "Z"
      formatSubseconds = formatTime defaultTimeLocale "%q"
  in formatTime defaultTimeLocale format utc

formatLocalTime :: LocalTime -> String
formatLocalTime locT = formatTime defaultTimeLocale "%FT%T" locT

formatDay :: Day -> String
formatDay dy = show dy

formatTimeOfDay :: TimeOfDay -> String
formatTimeOfDay tod = formatTime defaultTimeLocale "%T" tod


instance ToWidget LocalTime where
  toWidget wDef = do
    let def' = def
                & textInputConfig_inputType .~ "datetime-local"
                & attributes .~ (constDyn $ M.fromList [("class", "text-box"), ("step", "1")])
        timeDef = maybe def' (\a -> def' & textInputConfig_initialValue .~ (T.pack $ formatLocalTime a)) wDef
    txt <- textInput timeDef
    mapDyn (parseLocalTime . T.unpack) $ _textInput_value txt

instance ToWidget Day where
  toWidget wDef = do
    let def' = def
                & textInputConfig_inputType .~ "date"
                & attributes .~ constDyn ("class" =: "text-box")
        dayDef = maybe def' (\a -> def' & textInputConfig_initialValue .~ (T.pack $ formatDay a)) wDef
    txt <- textInput dayDef
    mapDyn (parseDay . T.unpack) $ _textInput_value txt

instance ToWidget TimeOfDay where
  toWidget wDef = do
    let def' = def
                & textInputConfig_inputType .~ "time"
                & attributes .~ (constDyn $ M.fromList [("class", "text-box"), ("step", "1")])
        todDef = maybe def' (\a -> def' & textInputConfig_initialValue .~ (T.pack $ formatTimeOfDay a)) wDef
    txt <- textInput todDef
    mapDyn (parseTimeOfDay . T.unpack) $ _textInput_value txt

instance ToWidget a => ToWidget [a] where
  toWidget _ = do
    divClass "list-wrapper" $ do
      rec dynValMap <- listWithKeyShallowDiff (M.empty :: M.Map Int ()) (leftmost evtList) createWidget
          let setNothingAt i = do
                valMap <- sample. current $ dynValMap
                return $ Just $ M.mapWithKey (\k a -> if k == i then Nothing else Just ()) valMap
              addElement = do
                valMap <- sample. current $ dynValMap
                lastKey <- sample . current $ lastKeyD
                return $ M.insert (lastKey + 1) (Just ()) $ M.map (const (Just ())) valMap
          dynListWithKeys <- mapDyn M.toList dynValMap
          dynValMap' <- mapDyn (M.map fst) dynValMap
          let getLastKey (x :: [Int]) = if null x then (-1) else maximum x
          lastKeyD <- mapDyn (getLastKey . map fst) dynListWithKeys
          (_, evtsD) <- splitDyn =<< mapDyn (unzip . map snd) dynListWithKeys
          let modelD = joinDynThroughMap dynValMap'
          evts <- mapDyn leftmost evtsD -- Remove events
          let evtList = (tag (pull addElement) addEvt) : [(push setNothingAt $ switchPromptlyDyn evts)]
          (addEvtEl, _) <- elAttr' "span" ("class" =: "plus-button") $ text "+"
          let addEvt = _el_clicked addEvtEl
      mapDyn (sequence . (map snd) . M.toList) modelD
    where
      fn :: (Reflex t, MonadSample t m) => [Dynamic t (Maybe a)] -> m (Maybe [a])
      fn = (\model -> do
        model' <- mapM (sample . current) model
        return $ sequence model'
        )
      createWidget k _ _ = initNew k
      initNew :: (MonadWidget t m , ToWidget a) => Int -> m (Dynamic t (Maybe a), Event t Int)
      initNew i = do
        mDyn   <- toWidget Nothing
        (removeEl, _) <- elAttr' "span" ("class" =: "cross-button") $ text "+"
        let onRemove = _el_clicked removeEl
        mDyn' <- mapDyn (maybe [] (: [])) mDyn
        return (mDyn, tag (constant i) onRemove)

instance ToWidget a => ToWidget (Maybe a) where
  toWidget _ = do
    divClass "maybe-wrapper" $ do
      let checkboxDefVal = False
      chk <- checkbox checkboxDefVal def
      widget <- toWidget Nothing
      let checkboxDyn = _checkbox_value chk
      isActive <- toggle checkboxDefVal (updated checkboxDyn)
      combineDyn (\a b -> fmap (\x -> if a then Just x else Nothing) b) isActive widget


type DynamicAttr t = Dynamic t (Map Text Text)

data GToWidgetState t = GToWidgetState
  { st_constructors :: (Event t Text, Map Text (DynamicAttr t))
  }

data GToWidgetOpts t f a = GToWidgetOpts
  { state        :: Maybe (GToWidgetState t)
  , widgetDefVal :: Maybe (f a)
  , arbitraryDef :: Maybe (Dynamic t (f a))
  }

class GToWidget f where
  gToWidget :: ( MonadWidget t m
               ) => GToWidgetOpts t f a -> m (Dynamic t (Maybe (f a)))


instance (GToWidget f, CtorInfo f) => GToWidget (D1 c f) where
  gToWidget gOpts@(GToWidgetOpts _ wDef aDef) = do
    {-wDef' <- case wDef of
      Just (D1 x) -> Just x
      _ -> Nothing-}
    let ctorNames = constructorNames (Proxy :: Proxy f)
    aDef' <- case aDef of
      Just dynM1 -> Just <$> mapDyn (\(M1 a) -> a) dynM1
      _           -> return Nothing
    let wDef' = fmap (\(M1 a) -> a) wDef
        gopts' = GToWidgetOpts Nothing wDef' aDef'
    case ctorNames of
      (firstCtor:_:_) -> do  -- SumType
        divClass "sum-wrapper" $ do
          let ctorNameMap = M.fromList $ map (\x -> (x, x)) ctorNames
          dd <- dropdown firstCtor (constDyn ctorNameMap) $ def
          sumTyAttrMap <- (return . M.fromList) =<< mapM (\c -> do
            cDyn <- mapDyn (\ddVal -> if ddVal == c then ("class" =: "sum-ty active") else ("class" =: "sum-ty")) (_dropdown_value dd)
            return (c, cDyn)
            ) ctorNames
          mapDyn (fmap M1) =<< gToWidget gopts' { state = Just $ GToWidgetState (_dropdown_change dd, sumTyAttrMap) }
      _ -> mapDyn (fmap M1) =<< gToWidget gopts'

instance (GToWidget f, GToWidget g, CtorInfo f, GToWidget (g :+: h)) => GToWidget (f :+: g :+: h) where
  gToWidget gopts@(GToWidgetOpts gstate wDef aDef) = do
    let (evt, attrMap) =
          case gstate of
            Just (GToWidgetState st) -> st
            _                        -> (never, M.empty)
        lConName         = head $ constructorNames (Proxy :: Proxy f)
        lDynAttr = fromMaybe (error $ "PANIC!: Constructor lookup failed @ GToWidget (f :+: g)" ++ (T.unpack lConName) ++ (show $ M.keys attrMap)) (M.lookup lConName attrMap)
        (lwDef, rwDef) = case wDef of
          Just (L1 x) -> (Just x, Nothing)
          Just (R1 x) -> (Nothing, Just x)
          _ -> (Nothing, Nothing)

    lDyn <- mapDyn (fmap L1) =<< do
      elDynAttr "div" lDynAttr $ do
        gToWidget (GToWidgetOpts (Just $ GToWidgetState ({-M.delete lConName-} (evt, attrMap))) lwDef Nothing)
    rDyn <- mapDyn (fmap R1) =<< gToWidget (GToWidgetOpts (Just $ GToWidgetState ({-M.delete lConName-} (evt, attrMap))) rwDef Nothing)

    fmap joinDyn $ foldDyn (\a _ -> if a == lConName then lDyn else rDyn) lDyn evt

instance (GToWidget f, GToWidget g, Typeable g, CtorInfo f, Constructor c) => GToWidget (f :+: C1 c g) where
  gToWidget gopts@(GToWidgetOpts gstate wDef aDef) = do
    let (evt, attrMap) =
          case gstate of
            Just (GToWidgetState st) -> st
            _                        -> (never, M.empty)
        lConName         = head $ constructorNames (Proxy :: Proxy f)
        lDynAttr = fromMaybe (error $ "PANIC!: Constructor lookup failed @ GToWidget (f :+: C1 c f)" ++ (T.unpack lConName) ++ (show $ M.keys attrMap)) (M.lookup lConName attrMap)
        rConName         = T.pack $ conName (undefined :: t c g a)
        rDynAttr = fromMaybe (error $ "PANIC!: Constructor lookup failed @ GToWidget (f :+: C1 c f)" ++ (T.unpack lConName) ++ (show $ M.keys attrMap)) (M.lookup rConName attrMap)
        (lwDef, rwDef) = case wDef of
          Just (L1 x) -> (Just x, Nothing)
          Just (R1 x) -> (Nothing, Just x)
          _ -> (Nothing, Nothing)
    lDyn <- mapDyn (fmap L1) =<< do
      elDynAttr "div" lDynAttr $ do
        gToWidget (GToWidgetOpts (Just $ GToWidgetState ({-M.delete lConName-} evt, attrMap)) lwDef Nothing)
    rDyn <- mapDyn (fmap R1) =<< do
      elDynAttr "div" rDynAttr $ do
        gToWidget (GToWidgetOpts Nothing rwDef Nothing)

    fmap joinDyn $ foldDyn (\a _ -> if a == lConName then lDyn else rDyn) lDyn evt

instance (GToWidget a, GToWidget b) => GToWidget (a :*: b) where
  gToWidget gopts@(GToWidgetOpts _ wDef _)= do
    let (awDef, bwDef) = case wDef of
          Nothing -> (Nothing, Nothing)
          Just (ad :*: bd) -> (Just ad, Just bd)
        aGopts' = GToWidgetOpts Nothing awDef Nothing
        bGopts' = GToWidgetOpts Nothing bwDef Nothing
    adyn <- gToWidget aGopts'
    bdyn <- gToWidget bGopts'
    combineDyn (\a b -> (:*:) <$> a <*> b) adyn bdyn

instance (GToWidget f, Typeable f, Constructor c) => GToWidget (C1 c f) where
  gToWidget gopts@(GToWidgetOpts _ wDef _) = do
    let wDef' = fmap (\(M1 a) -> a) wDef
        gopts' = GToWidgetOpts Nothing wDef' Nothing
    case eqT :: Maybe (f :~: U1) of
      Just Refl -> do
        mapDyn (fmap M1) =<< (gToWidget gopts')
      _ -> do
        elClass "fieldset" "nested-field field" $ do
          el "legend" $ text $ T.pack $ conName (undefined :: C1 c f ())
          divClass "field" $ mapDyn (fmap M1) =<< (gToWidget gopts')

instance GToWidget U1 where
  gToWidget _ = return $ constDyn (Just U1)

instance GToWidget V1 where
  gToWidget _ = return $ constDyn (error "PANIC!: Unreachable code")

instance (GToWidget f, Selector s) => GToWidget (S1 s f) where
  gToWidget gopts@(GToWidgetOpts _ wDef _) = do
    let wDef' = fmap (\(M1 a) -> a) wDef
        gopts' = GToWidgetOpts Nothing wDef' Nothing
    elClass "div" "field" $ do
      elAttr "label" ("class" =: "label") $ text $ T.pack $ selName (undefined :: S1 s f ())
      inp <- gToWidget gopts'
      mapDyn (fmap M1) inp

instance (ToWidget f) => GToWidget (K1 c f) where
  gToWidget (GToWidgetOpts _ wDef _) =
    let wDef' = fmap (\(K1 a) -> a) wDef
    in mapDyn (fmap K1) =<< toWidget wDef'


class CtorInfo (f :: * -> *) where
  constructorNames :: proxy f -> [Text]

instance CtorInfo f => CtorInfo (D1 c f) where
  constructorNames _ = constructorNames (Proxy :: Proxy f)

instance (CtorInfo x, CtorInfo y) => CtorInfo (x :+: y) where
  constructorNames _ = constructorNames (Proxy :: Proxy x) ++ constructorNames (Proxy :: Proxy y)

instance (Constructor c) => CtorInfo (C1 c f) where
  constructorNames _ = [T.pack $ conName (undefined :: t c f a)]
