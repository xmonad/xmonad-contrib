{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationEx.LayoutModifier
-- Description :  Layout modifier which adds decorations to windows.
-- Copyright   :  (c) 2007 Andrea Rossato, 2009 Jan Vornberger, 2023 Ilya Portnov
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout modifier, which is responsible for creation of decoration rectangles
-- (windows), updating and removing them when needed. It is parametrized by
-- @DecorationGeometry@, which says where decorations should be placed, and by
-- @DecorationEngine@, which says how decorations should look.
-----------------------------------------------------------------------------

module XMonad.Layout.DecorationEx.LayoutModifier (
    -- * Usage
    --
    -- $usage
    decorationEx,
    DecorationEx
  ) where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowArranger (diff, listFromList)
import XMonad.Util.Invisible
import XMonad.Util.XUtils hiding (paintTextAndIcons)

import XMonad.Layout.DecorationEx.Common
import XMonad.Layout.DecorationEx.Engine
import XMonad.Layout.DecorationEx.Geometry

-- $usage
--
-- This module exports @decorationEx@ function, which is a generic function for
-- adding decorations to your layouts. It can be used to use different
-- decoration geometries and engines in any combination.
-- For most used combinations, there are convinience functions in
-- "XMonad.Layout.DecorationEx.TextEngine", "XMonad.Layout.DecorationEx.TabbedGeometry",
-- and "XMonad.Layout.DecorationEx.DwmGeometry".
--
-- You can use this module with the following in your
-- @xmonad.hs@:
--
-- > import XMonad.Layout.DecorationEx.LayoutModifier
-- Then edit your @layoutHook@ by adding the DwmStyle decoration to
-- your layout:
--
-- > myL = decorationEx shrinkText myTheme myEngine myGeometry (layoutHook def)
-- >         where
-- >           myGeometry = DefaultGeometry -- or another geometry type
-- >           myEngine = TextDecoration    -- or another decoration engine
-- >           myTheme = GenericTheme {...} -- theme type should correspond to selected engine type
-- >
-- > main = xmonad def { layoutHook = myL }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"


-- | The 'DecorationEx' 'LayoutModifier'. This data type is an instance
-- of the 'LayoutModifier' class. This data type will be passed,
-- together with a layout, to the 'ModifiedLayout' type constructor
-- to modify the layout by adding decorations according to a
-- 'DecorationEngine'.
data DecorationEx engine widget geom shrinker a =
    DecorationEx (Invisible Maybe (DecorationLayoutState engine)) shrinker (Theme engine widget) (engine widget a) (geom a)

deriving instance (Show (Theme engine widget), Show shrinker, Show (engine widget a), Show (geom a)) => Show (DecorationEx engine widget geom shrinker a)
deriving instance (Read (Theme engine widget), Read shrinker, Read (engine widget a), Read (geom a)) => Read (DecorationEx engine widget geom shrinker a)

-- | The long 'LayoutModifier' instance for the 'DecorationEx' type.
--
-- In 'redoLayout' we check the state: if there is no state we
-- initialize it.
--
-- The state is @diff@ed against the list of windows produced by the
-- underlying layout: removed windows get deleted and new ones
-- decorated by 'createDecos', which will call 'decorate' to decide if
-- a window must be given a 'Rectangle', in which case a decoration
-- window will be created.
--
-- After that we resync the updated state with the windows' list and
-- then we process the resynced stated (as we do with a new state).
--
-- First we map the decoration windows, we update each decoration to
-- reflect any decorated window's change, and we insert, in the list
-- of windows and rectangles returned by the underlying layout, the
-- decoration for each window. This way xmonad will restack the
-- decorations and their windows accordingly. At the end we remove
-- invisible\/stacked windows.
--
-- Message handling is quite simple: when needed we release the state
-- component of the 'DecorationEx' 'LayoutModifier'. Otherwise we call
-- 'handleEvent', which will call the appropriate 'DecorationEngine'
-- methods to perform its tasks.
instance (DecorationEngine engine widget Window, DecorationGeometry geom Window, Shrinker shrinker) => LayoutModifier (DecorationEx engine widget geom shrinker) Window where
    redoLayout (DecorationEx (I (Just decoState)) shrinker theme engine geom) _ Nothing _ = do
        releaseResources engine decoState
        return ([], Just $ DecorationEx (I Nothing) shrinker theme engine geom)
    redoLayout _ _ Nothing _  = return ([], Nothing)

    redoLayout (DecorationEx invState shrinker theme engine geom) screenRect (Just stack) srcPairs
        | I Nothing  <- invState = initState theme engine geom shrinker screenRect stack srcPairs >>= processState
        | I (Just s) <- invState = do
            let decorations  = dsDecorations s
                (d,a) = curry diff (getOrigWindows decorations) srcWindows
                toDel = todel d decorations
                toAdd = toadd a srcPairs
            deleteDecos toDel
            let decosToBeAdded = [WindowDecoration win rect Nothing Nothing [] | (win, rect) <- toAdd]
            newDecorations <- resync (dsStyleState s) (decosToBeAdded ++ del_dwrs d decorations) srcPairs
            processState (s {dsDecorations = newDecorations})

        where
          srcWindows = map fst srcPairs

          getOrigWindows :: [WindowDecoration] -> [Window]
          getOrigWindows = map wdOrigWindow

          del_dwrs :: [Window] -> [WindowDecoration] -> [WindowDecoration]
          del_dwrs = listFromList wdOrigWindow notElem

          findDecoWindow :: Int -> [WindowDecoration] -> Maybe Window
          findDecoWindow i d = wdDecoWindow $ d !! i

          todel :: [Window] -> [WindowDecoration] -> [WindowDecoration]
          todel d = filter (\dd -> wdOrigWindow dd `elem` d)

          toadd :: [Window] -> [(Window, Rectangle)] -> [(Window, Rectangle)]
          toadd a = filter (\p -> fst p `elem` a)

          createDecoWindowIfNeeded :: Maybe Window -> Maybe Rectangle -> X (Maybe Window)
          createDecoWindowIfNeeded mbDecoWindow mbDecoRect =
            case (mbDecoWindow, mbDecoRect) of
              (Nothing, Just decoRect) -> do
                decoWindow <- createDecoWindow engine theme decoRect
                return $ Just decoWindow
              _ -> return mbDecoWindow

          resync :: DecorationEngineState engine -> [WindowDecoration] -> [(Window,Rectangle)] -> X [WindowDecoration]
          resync _ _ [] = return []
          resync decoState dd ((window,rect):xs) =
            case  window `elemIndex` getOrigWindows dd of
              Just i  -> do
                mbDecoRect <- decorateWindow geom screenRect stack srcPairs (window,rect)
                widgetPlaces <- case mbDecoRect of
                                  Nothing -> return $ WidgetLayout [] [] []
                                  Just decoRect -> placeWidgets engine theme shrinker decoState decoRect window (themeWidgets theme)
                mbDecoWindow  <- createDecoWindowIfNeeded (findDecoWindow i dd) mbDecoRect
                let newDd = WindowDecoration window rect mbDecoWindow mbDecoRect (widgetLayout widgetPlaces)
                restDd <- resync decoState dd xs
                return $ newDd : restDd
              Nothing -> resync decoState dd xs

          -- We drop any windows that are *precisely* stacked underneath
          -- another window: these must be intended to be tabbed!
          removeTabbed :: [Rectangle] -> [(Window, Rectangle)] -> [(Window, Rectangle)]
          removeTabbed _ [] = []
          removeTabbed rs ((w,r):xs)
              | r `elem` rs = removeTabbed rs xs
              | otherwise   = (w,r) : removeTabbed (r:rs) xs

          insertDwr :: WindowDecoration -> [(Window, Rectangle)] -> [(Window, Rectangle)]
          insertDwr dd wrs =
            case (wdDecoWindow dd, wdDecoRect dd) of
              (Just decoWindow, Just decoRect) -> (decoWindow, decoRect) : (wdOrigWindow dd, shrinkWindow geom decoRect (wdOrigWinRect dd)) : wrs
              _ -> (wdOrigWindow dd, wdOrigWinRect dd) : wrs

          dwrs_to_wrs :: [WindowDecoration] -> [(Window, Rectangle)]
          dwrs_to_wrs = removeTabbed [] . foldr insertDwr []

          processState :: DecorationLayoutState engine -> X ([(Window, Rectangle)], Maybe (DecorationEx engine widget geom shrinker Window))
          processState st = do
            let decorations = dsDecorations st
            showDecos decorations
            updateDecos engine shrinker theme (dsStyleState st) decorations
            return (dwrs_to_wrs decorations, Just (DecorationEx (I (Just (st {dsDecorations = decorations}))) shrinker theme engine geom))

    handleMess (DecorationEx (I (Just st)) shrinker theme engine geom) m
        | Just e <- fromMessage m = do
            decorationEventHookEx engine theme st shrinker e
            handleEvent engine shrinker theme st e
            return Nothing
        | Just Hide <- fromMessage m = do
            hideDecos $ dsDecorations st
            return Nothing
--         | Just (SetTheme nt) <- fromMessage m = do
--             releaseResources engine st
--             let t' = themeEx nt
--             return $ Just $ DecorationEx (I Nothing) shrinker t' engine
        | Just ReleaseResources <- fromMessage m = do
            releaseResources engine st
            return $ Just $ DecorationEx (I Nothing) shrinker theme  engine geom
    handleMess _ _ = return Nothing

    modifierDescription (DecorationEx _ _ _ engine geom) = describeEngine engine ++ describeGeometry geom

-- | By default 'DecorationEx' handles 'PropertyEvent' and 'ExposeEvent'
-- only.
handleEvent :: (Shrinker shrinker, DecorationEngine engine widget Window) => engine widget Window -> shrinker -> Theme engine widget -> DecorationLayoutState engine -> Event -> X ()
handleEvent engine shrinker theme (DecorationLayoutState {..}) e
    | PropertyEvent {ev_window = w, ev_atom = atom} <- e
    , Just i <- w `elemIndex` map wdOrigWindow dsDecorations = do
        supportedAtoms <- propsToRepaintDecoration engine
        when (atom `elem` supportedAtoms) $ do
          -- io $ putStrLn $ "property event on " ++ show w -- ++ ": " ++ fromMaybe "<?>" atomName
          updateDeco engine shrinker theme dsStyleState (dsDecorations !! i) False
    | ExposeEvent   {ev_window = w} <- e
    , Just i <- w `elemIndex` mapMaybe wdDecoWindow dsDecorations = do
        -- io $ putStrLn $ "expose event on " ++ show w
        updateDeco engine shrinker theme dsStyleState (dsDecorations !! i) True
handleEvent _ _ _ _ _ = return ()

-- | Initialize the 'DecorationState' by initializing the font
-- structure and by creating the needed decorations.
initState :: (DecorationEngine engine widget Window, DecorationGeometry geom Window, Shrinker shrinker)
          => Theme engine widget
          -> engine widget Window
          -> geom Window
          -> shrinker
          -> Rectangle
          -> W.Stack Window
          -> [(Window,Rectangle)] -> X (DecorationLayoutState engine)
initState theme engine geom shrinker screenRect stack wrs = do
  styleState <- initializeState engine geom theme
  decorations <- createDecos theme engine geom shrinker styleState screenRect stack wrs wrs
  return $ DecorationLayoutState styleState decorations

-- | Delete windows stored in the state and release the font structure.
releaseResources :: DecorationEngine engine widget Window => engine widget Window -> DecorationLayoutState engine -> X ()
releaseResources engine st = do
  deleteDecos (dsDecorations st)
  releaseStateResources engine (dsStyleState st)

-- | Create the decoration windows of a list of windows and their
-- rectangles, by calling the 'decorate' method of the
-- 'DecorationStyle' received.
createDecos :: (DecorationEngine engine widget Window, DecorationGeometry geom Window, Shrinker shrinker)
            => Theme engine widget
            -> engine widget Window
            -> geom Window
            -> shrinker
            -> DecorationEngineState engine
            -> Rectangle
            -> W.Stack Window
            -> [(Window,Rectangle)] -> [(Window,Rectangle)] -> X [WindowDecoration]
createDecos theme engine geom shrinker decoState screenRect stack wrs ((w,r):xs) = do
  mbDecoRect <- decorateWindow geom screenRect stack wrs (w,r)
  case mbDecoRect of
    Just decoRect -> do
      decoWindow <- createDecoWindow engine theme decoRect
      widgetPlaces <- placeWidgets engine theme shrinker decoState decoRect w (themeWidgets theme)
      restDd <- createDecos theme engine geom shrinker decoState screenRect stack wrs xs
      let newDd = WindowDecoration w r (Just decoWindow) (Just decoRect) $ widgetLayout widgetPlaces
      return $ newDd : restDd
    Nothing -> do
      restDd <- createDecos theme engine geom shrinker decoState screenRect stack wrs xs
      let newDd = WindowDecoration w r Nothing Nothing []
      return $ newDd : restDd
createDecos _ _ _ _ _ _ _ _ [] = return []

createDecoWindow :: (DecorationEngine engine widget Window) => engine widget Window -> Theme engine widget -> Rectangle -> X Window
createDecoWindow engine theme rect = do
  let mask = Just $ decorationXEventMask engine
  w <- createNewWindow rect mask (defaultBgColor theme) True
  d <- asks display
  io $ setClassHint d w (ClassHint "xmonad-decoration" "xmonad")
  return w

showDecos :: [WindowDecoration] -> X ()
showDecos dd =
  showWindows $ mapMaybe wdDecoWindow $ filter (isJust . wdDecoRect) dd

hideDecos :: [WindowDecoration] -> X ()
hideDecos = hideWindows . mapMaybe wdDecoWindow

deleteDecos :: [WindowDecoration] -> X ()
deleteDecos = deleteWindows . mapMaybe wdDecoWindow

updateDecos :: (Shrinker shrinker, DecorationEngine engine widget Window)
            => engine widget Window -> shrinker -> Theme engine widget -> DecorationEngineState engine -> [WindowDecoration] -> X ()
updateDecos engine shrinker theme decoState = mapM_ (\wd -> updateDeco engine shrinker theme decoState wd False)

-- | Update a decoration window given a shrinker, a theme, the font
-- structure and the needed 'Rectangle's
updateDeco :: (Shrinker shrinker, DecorationEngine engine widget Window) => engine widget Window -> shrinker -> Theme engine widget -> DecorationEngineState engine -> WindowDecoration -> Bool -> X ()
updateDeco engine shrinker theme decoState wd isExpose =
  case (wdDecoWindow wd, wdDecoRect wd) of
    (Just decoWindow, Just decoRect@(Rectangle _ _ wh ht)) -> do
      let origWin = wdOrigWindow wd
      drawData <- mkDrawData engine theme decoState origWin decoRect
      widgetPlaces <- placeWidgets engine theme shrinker decoState decoRect (wdOrigWindow wd) (themeWidgets theme)
      -- io $ print widgetPlaces
      paintDecoration engine decoWindow wh ht shrinker (drawData {ddWidgetPlaces = widgetPlaces}) isExpose
    (Just decoWindow, Nothing) -> hideWindow decoWindow
    _ -> return ()

-- | Apply a DecorationEx modifier to an underlying layout
decorationEx :: (DecorationEngine engine widget a, DecorationGeometry geom a, Shrinker shrinker)
             => shrinker             -- ^ Strings shrinker, for example @shrinkText@
             -> Theme engine widget  -- ^ Decoration theme
             -> engine widget a      -- ^ Decoration engine instance
             -> geom a               -- ^ Decoration geometry instance
             -> l a                  -- ^ Underlying layout to be decorated
             -> ModifiedLayout (DecorationEx engine widget geom shrinker) l a
decorationEx shrinker theme engine geom = ModifiedLayout (DecorationEx (I Nothing) shrinker theme engine geom)

