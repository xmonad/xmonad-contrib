--------------------------------------------------------------------------------
-- | Example.hs
--
-- Example configuration file for xmonad using the latest recommended
-- features (e.g., 'desktopConfig').
module Main (main) where

--------------------------------------------------------------------------------
import System.Exit
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig

--------------------------------------------------------------------------------
main = do
  spawn "xmobar" -- Start a task bar such as xmobar.

  -- Start xmonad using the main desktop configuration with a few
  -- simple overrides:
  xmonad $ desktopConfig
    { modMask    = mod4Mask -- Use the "Win" key for the mod key
    , manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = desktopLayoutModifiers myLayouts
    , logHook    = (dynamicLogString def >>= xmonadPropLog)
                    <+> logHook desktopConfig
    }

    `additionalKeysP` -- Add some extra key bindings:
      [ ("M-S-q",   confirmPrompt myXPConfig "exit" (io exitSuccess))
      , ("M-p",     shellPrompt myXPConfig)
      , ("M-<Esc>", sendMessage (Toggle "Full"))
      ]

--------------------------------------------------------------------------------
-- | Customize layouts.
--
-- This layout configuration uses two primary layouts, 'ResizableTall'
-- and 'BinarySpacePartition'.  You can also use the 'M-<Esc>' key
-- binding defined above to toggle between the current layout and a
-- full screen layout.
myLayouts = toggleLayouts (noBorders Full) others
  where
    others = ResizableTall 1 (1.5/100) (3/5) [] ||| emptyBSP

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:monospace:size=9"
  }

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook = composeOne
  -- Handle floating windows:
  [ transience            -- move transient windows to their parent
  , isDialog              -?> doCenterFloat
  ] <+> composeAll
  [ className =? "Pidgin" --> doFloat
  , className =? "XCalc"  --> doFloat
  , className =? "mpv"    --> doFloat
  ]
