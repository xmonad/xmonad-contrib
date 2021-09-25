-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.TagWindows
-- Description  : Functions for tagging windows and selecting them by tags.
-- Copyright    : (c) Karsten Schoelzel <kuser@gmx.de>
-- License      : BSD
--
-- Maintainer   : Karsten Schoelzel <kuser@gmx.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- Functions for tagging windows and selecting them by tags.
-----------------------------------------------------------------------------

module XMonad.Actions.TagWindows (
                 -- * Usage
                 -- $usage
                 addTag, delTag, unTag,
                 setTags, getTags, hasTag,
                 withTaggedP,  withTaggedGlobalP, withFocusedP,
                 withTagged ,  withTaggedGlobal ,
                 focusUpTagged,   focusUpTaggedGlobal,
                 focusDownTagged, focusDownTaggedGlobal,
                 shiftHere, shiftToScreen,
                 tagPrompt,
                 tagDelPrompt,
                 TagPrompt,
                 ) where

import Control.Exception as E

import XMonad hiding (workspaces)
import XMonad.Prelude
import XMonad.Prompt
import XMonad.StackSet hiding (filter)

econst :: Monad m => a -> IOException -> m a
econst = const . return

-- $usage
--
-- To use window tags, import this module into your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.TagWindows
-- > import XMonad.Prompt    -- to use tagPrompt
--
-- and add keybindings such as the following:
--
-- >   , ((modm,                 xK_f  ), withFocused (addTag "abc"))
-- >   , ((modm .|. controlMask, xK_f  ), withFocused (delTag "abc"))
-- >   , ((modm .|. shiftMask,   xK_f  ), withTaggedGlobalP "abc" W.sink)
-- >   , ((modm,                 xK_d  ), withTaggedP "abc" (W.shiftWin "2"))
-- >   , ((modm .|. shiftMask,   xK_d  ), withTaggedGlobalP "abc" shiftHere)
-- >   , ((modm .|. controlMask, xK_d  ), focusUpTaggedGlobal "abc")
-- >   , ((modm,                 xK_g  ), tagPrompt def (\s -> withFocused (addTag s)))
-- >   , ((modm .|. controlMask, xK_g  ), tagDelPrompt def)
-- >   , ((modm .|. shiftMask,   xK_g  ), tagPrompt def (\s -> withTaggedGlobal s float))
-- >   , ((modWinMask,                xK_g  ), tagPrompt def (\s -> withTaggedP s (W.shiftWin "2")))
-- >   , ((modWinMask .|. shiftMask,  xK_g  ), tagPrompt def (\s -> withTaggedGlobalP s shiftHere))
-- >   , ((modWinMask .|. controlMask, xK_g ), tagPrompt def (\s -> focusUpTaggedGlobal s))
--
-- NOTE: Tags are saved as space separated strings and split with
--       'unwords'. Thus if you add a tag \"a b\" the window will have
--       the tags \"a\" and \"b\" but not \"a b\".
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | set multiple tags for a window at once (overriding any previous tags)
setTags :: [String] -> Window -> X ()
setTags = setTag . unwords

-- | set a tag for a window (overriding any previous tags)
--   writes it to the \"_XMONAD_TAGS\" window property
setTag :: String -> Window -> X ()
setTag s w = withDisplay $ \d ->
    io $ internAtom d "_XMONAD_TAGS" False >>= setTextProperty d w s

-- | read all tags of a window
--   reads from the \"_XMONAD_TAGS\" window property
getTags :: Window -> X [String]
getTags w = withDisplay $ \d ->
    io $ E.catch (internAtom d "_XMONAD_TAGS" False >>=
                getTextProperty d w >>=
                wcTextPropertyToTextList d)
               (econst [[]]) <&> (words . unwords)

-- | check a window for the given tag
hasTag :: String -> Window -> X Bool
hasTag s w = (s `elem`) <$> getTags w

-- | add a tag to the existing ones
addTag :: String -> Window -> X ()
addTag s w = do
    tags <- getTags w
    when (s `notElem` tags) $ setTags (s:tags) w

-- | remove a tag from a window, if it exists
delTag :: String -> Window -> X ()
delTag s w = do
    tags <- getTags w
    setTags (filter (/= s) tags) w

-- | remove all tags
unTag :: Window -> X ()
unTag = setTag ""

-- | Move the focus in a group of windows, which share the same given tag.
--   The Global variants move through all workspaces, whereas the other
--   ones operate only on the current workspace
focusUpTagged, focusDownTagged, focusUpTaggedGlobal, focusDownTaggedGlobal :: String -> X ()
focusUpTagged         = focusTagged' (reverse . wsToList)
focusDownTagged       = focusTagged' wsToList
focusUpTaggedGlobal   = focusTagged' (reverse . wsToListGlobal)
focusDownTaggedGlobal = focusTagged' wsToListGlobal

wsToList :: (Ord i) => StackSet i l a s sd -> [a]
wsToList ws = crs ++ cls
    where
        (crs, cls) = (cms down, cms (reverse . up))
        cms f = maybe [] f (stack . workspace . current $ ws)

wsToListGlobal :: (Ord i) => StackSet i l a s sd -> [a]
wsToListGlobal ws = concat ([crs] ++ rws ++ lws ++ [cls])
    where
        curtag = currentTag ws
        (crs, cls) = (cms down, cms (reverse . up))
        cms f = maybe [] f (stack . workspace . current $ ws)
        (lws, rws) = (mws (<), mws (>))
        mws cmp = map (integrate' . stack) . sortByTag . filter (\w -> tag w `cmp` curtag) . workspaces $ ws
        sortByTag = sortBy (\x y -> compare (tag x) (tag y))

focusTagged' :: (WindowSet -> [Window]) -> String -> X ()
focusTagged' wl t = gets windowset >>= findM (hasTag t) . wl >>=
    maybe (return ()) (windows . focusWindow)

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []      = return Nothing
findM p (x:xs)  = do b <- p x
                     if b then return (Just x) else findM p xs

-- | apply a pure function to windows with a tag
withTaggedP, withTaggedGlobalP :: String -> (Window -> WindowSet -> WindowSet) -> X ()
withTaggedP       t f = withTagged'       t (winMap f)
withTaggedGlobalP t f = withTaggedGlobal' t (winMap f)

winMap :: (Window -> WindowSet -> WindowSet) -> [Window] -> X ()
winMap f tw = when (tw /= []) (windows $ foldl1 (.) (map f tw))

withTagged, withTaggedGlobal :: String -> (Window -> X ()) -> X ()
withTagged       t f = withTagged'       t (mapM_ f)
withTaggedGlobal t f = withTaggedGlobal' t (mapM_ f)

withTagged' :: String -> ([Window] -> X ()) -> X ()
withTagged' t m = gets windowset >>= filterM (hasTag t) . index >>= m

withTaggedGlobal' :: String -> ([Window] -> X ()) -> X ()
withTaggedGlobal' t m = gets windowset >>=
    filterM (hasTag t) . concatMap (integrate' . stack) . workspaces >>= m

withFocusedP :: (Window -> WindowSet -> WindowSet) -> X ()
withFocusedP f = withFocused $ windows . f

shiftHere :: (Ord a, Eq s, Eq i) => a -> StackSet i l a s sd -> StackSet i l a s sd
shiftHere w s = shiftWin (currentTag s) w s

shiftToScreen :: (Ord a, Eq s, Eq i) => s -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftToScreen sid w s = case filter (\m -> sid /= screen m) (current s:visible s) of
                                []      -> s
                                (t:_)   -> shiftWin (tag . workspace $ t) w s

data TagPrompt = TagPrompt

instance XPrompt TagPrompt where
    showXPrompt TagPrompt = "Select Tag:   "


tagPrompt :: XPConfig -> (String -> X ()) -> X ()
tagPrompt c f = do
  sc <- tagComplList
  mkXPrompt TagPrompt c (mkComplFunFromList' c sc) f

tagComplList :: X [String]
tagComplList = gets (concatMap (integrate' . stack) . workspaces . windowset)
           >>= mapM getTags
           <&> nub . concat


tagDelPrompt :: XPConfig -> X ()
tagDelPrompt c = do
  sc <- tagDelComplList
  when (sc /= []) $
    mkXPrompt TagPrompt c (mkComplFunFromList' c sc) (withFocused . delTag)

tagDelComplList :: X [String]
tagDelComplList = gets windowset >>= maybe (return []) getTags . peek
