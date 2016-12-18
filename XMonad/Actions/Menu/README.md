# XMonad.Actions.Menu: Helm-like Menus for XMonad

Having recently switched to Spacemacs from Emacs (since Vim-like text-editing is
wonderful, but emacs is a great OS) I've found that the way in which they handle
commands has been very helpful in discovering new commands, and aids in
memorizing commands.

Commands are usually single keystrokes that either run a command or enter into a
submenu, which itself then contains more commands. As you descend into menus,
all the keybindings and commands are displayed in a temporary buffer at the
bottom of the editor.

I wanted to reproduce that behavior in XMonad.

## Defining Menus

The functionality for running Menus and rendering Menus to the screen is kept as
separate as possible, so to make use a menu you must at least import `runMenu`
from `XMonad.Actions.Menu.Core` and some type of Menu from one of the different
renderers. Currently, there is a Hidden menu (in `XMonad.Actions.Menu.Hidden`)
(which displays nothing at all) and a Dzen Menu (in `XMonad.Actions.Menu.Dzen`),
which uses Dzen to draw Menus to the screen.

## Generating nested menus

Menus bind key-bindings to X () actions and running a menu is simply an X ()
action, therefore nesting of menus is not represented at all in the code, and to
generate nested menus, just add a keybinding to `runMenu myMenu` in your other
menu.

## Example configuration

Below a bit of code showing some common patterns in defining these menus.

```haskell

-- More XMonad import go here

import XMonad.Actions.Menu.Core (runMenu)
import XMonad.Actions.Menu.Dzen
import XMonad.Actions.Menu.Hidden

-- More xmonad.hs code goes here

menuCfg :: DzenCfg
menuCfg  = def { bgCol = "#000000" }

submenu :: DzenItemCfg
submenu = def { descrCol = "#0000FF"}

caution :: DzenItemCfg
caution = def { descrCol = "#FF0000"}

mainMenu :: DzenMenu
mainMenu = dzenMenu menuCfg { name = "Main Menu" }
  [ ("<Escape>", "close menu"          , return ()                , def { hidden=True })
  , ("l"       , "launch applications" , runMenu launchMenu       , submenu)
  , ("a"       , "arrange window"      , runMenu arrangeMenu      , submenu)
  , ("k"       , "kill window"         , kill                     , def)
  , ("C-s"     , "shutdown"            , io $ exitWith ExitSuccess, caution { sepStr=" !! "} )]

launchMenu :: DzenMenu
launchMenu = dzenMenu menuCfg { name = "Launch programs", bgCol = "#220000"}
  [ ("<Escape>", "close menu" , return ()         , def { hidden=True })
  , ("c"       , "chromium"   , spawn "chromium"  , def)
  , ("g"       , "gimp"       , spawn "gimp"      , def) ]

arrangeMenu :: HiddenMenu
arrangeMenu = hiddenMenu
  [ ("<Escape>", "close menu"   , return ())
  , ("s"       , "Shrink master", (sendMessage Shrink >> runMenu arrangeMenu))
  , ("g"       , "Grow master"  , (sendMessage Expand >> runMenu arrangeMenu))
  ]

-- Then bind some key to `runMenu mainMenu` and voila!

```
