-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.DynamicWorkspaces
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides bindings to add and delete workspaces.  Note that you may only
-- delete a workspace that is already empty.
--
-----------------------------------------------------------------------------

module XMonad.Actions.DynamicWorkspaces (
                                         -- * Usage
                                         -- $usage
                                         addWorkspace, removeWorkspace,
                                         addHiddenWorkspace,
                                         withWorkspace,
                                         selectWorkspace, renameWorkspace,
                                         toNthWorkspace, withNthWorkspace
                                       ) where

import XMonad hiding (workspaces)
import XMonad.StackSet hiding (filter, modify, delete)
import XMonad.Prompt.Workspace
import XMonad.Prompt ( XPConfig, mkXPrompt, XPrompt(..) )
import XMonad.Util.WorkspaceCompare ( getSortByIndex )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Actions.DynamicWorkspaces
--
-- Then add keybindings like the following:
--
-- >   , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
-- >   , ((modm .|. shiftMask, xK_v      ), selectWorkspace defaultXPConfig)
-- >   , ((modm, xK_m                    ), withWorkspace defaultXPConfig (windows . W.shift))
-- >   , ((modm .|. shiftMask, xK_m      ), withWorkspace defaultXPConfig (windows . copy))
-- >   , ((modm .|. shiftMask, xK_r      ), renameWorkspace defaultXPConfig)
--
-- > -- mod-[1..9]       %! Switch to workspace N
-- > -- mod-shift-[1..9] %! Move client to workspace N
-- >    ++
-- >    zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
-- >    ++
-- >    zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".


data Wor = Wor String

instance XPrompt Wor where
    showXPrompt (Wor x) = x

mkCompl :: [String] -> String -> IO [String]
mkCompl l s = return $ filter (\x -> take (length s) x == s) l

withWorkspace :: XPConfig -> (String -> X ()) -> X ()
withWorkspace c job = do ws <- gets (workspaces . windowset)
                         sort <- getSortByIndex
                         let ts = map tag $ sort ws
                             job' t | t `elem` ts = job t
                                    | otherwise = addHiddenWorkspace t >> job t
                         mkXPrompt (Wor "") c (mkCompl ts) job'

renameWorkspace :: XPConfig -> X ()
renameWorkspace conf = workspacePrompt conf $ \w ->
                       windows $ \s -> let sett wk = wk { tag = w }
                                           setscr scr = scr { workspace = sett $ workspace scr }
                                           sets q = q { current = setscr $ current q }
                                       in sets $ removeWorkspace' w s

toNthWorkspace :: (String -> X ()) -> Int -> X ()
toNthWorkspace job wnum = do sort <- getSortByIndex
                             ws <- gets (map tag . sort . workspaces . windowset)
                             case drop wnum ws of
                               (w:_) -> job w
                               [] -> return ()

withNthWorkspace :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthWorkspace job wnum = do sort <- getSortByIndex
                               ws <- gets (map tag . sort . workspaces . windowset)
                               case drop wnum ws of
                                 (w:_) -> windows $ job w
                                 [] -> return ()

selectWorkspace :: XPConfig -> X ()
selectWorkspace conf = workspacePrompt conf $ \w ->
                       do s <- gets windowset
                          if tagMember w s
                            then windows $ greedyView w
                            else addWorkspace w

-- | Add a new workspace with the given name.
addWorkspace :: String -> X ()
addWorkspace newtag = addHiddenWorkspace newtag >> windows (greedyView newtag)


-- | Add a new hidden workspace with the given name.
addHiddenWorkspace :: String -> X ()
addHiddenWorkspace newtag = do l <- asks (layoutHook . config)
                               windows (addHiddenWorkspace' newtag l)

-- | Remove the current workspace if it contains no windows.
removeWorkspace :: X ()
removeWorkspace = do s <- gets windowset
                     case s of
                       StackSet { current = Screen { workspace = torem }
                                , hidden = (w:_) }
                           -> do windows $ view (tag w)
                                 windows (removeWorkspace' (tag torem))
                       _ -> return ()

addHiddenWorkspace' :: i -> l -> StackSet i l a sid sd -> StackSet i l a sid sd
addHiddenWorkspace' newtag l s@(StackSet { hidden = ws }) = s { hidden = Workspace newtag l Nothing:ws }

removeWorkspace' :: (Eq i) => i -> StackSet i l a sid sd -> StackSet i l a sid sd
removeWorkspace' torem s@(StackSet { current = scr@(Screen { workspace = wc })
                                   , hidden = (w:ws) })
    | tag w == torem = s { current = scr { workspace = wc { stack = meld (stack w) (stack wc) } }
                         , hidden = ws }
   where meld Nothing Nothing = Nothing
         meld x Nothing = x
         meld Nothing x = x
         meld (Just x) (Just y) = differentiate (integrate x ++ integrate y)
removeWorkspace' _ s = s
