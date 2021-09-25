-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DynamicHooks
-- Description :  One-shot and permanent ManageHooks that can be updated at runtime.
-- Copyright   :  (c) Braden Shepherdson 2008
-- License     :  BSD-style (as xmonad)
--
-- Maintainer  :  Braden.Shepherdson@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- One-shot and permanent ManageHooks that can be updated at runtime.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.DynamicHooks (
  -- * Usage
  -- $usage
  dynamicMasterHook
  ,addDynamicHook
  ,updateDynamicHook
  ,oneShotHook
  ) where

import XMonad
import XMonad.Prelude
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- Provides two new kinds of 'ManageHooks' that can be defined at runtime.
--
-- * One-shot 'ManageHooks' that are deleted after they execute.
--
-- * Permanent 'ManageHooks' (unless you want to destroy them)
--
-- Note that you will lose all dynamically defined 'ManageHook's when you @mod+q@!
-- If you want them to last, you should create them as normal in your @xmonad.hs@.
--
-- To use this module, add 'dynamicMasterHook' to your 'manageHook':
--
-- > xmonad { manageHook = myManageHook <+> dynamicMasterHook }
--
-- You can then use the supplied functions in your keybindings:
--
-- > ((modMask,xK_a), oneShotHook (className =? "example") doFloat)
--

data DynamicHooks = DynamicHooks
    { transients :: [(Query Bool, ManageHook)]
    , permanent  :: ManageHook }

instance ExtensionClass DynamicHooks where
    initialValue = DynamicHooks [] idHook

-- this hook is always executed, and the contents of the stored hooks checked.
-- note that transient hooks are run second, therefore taking precedence
-- over permanent ones on matters such as which workspace to shift to.
-- doFloat and doIgnore are idempotent.
-- | Master 'ManageHook' that must be in your @xmonad.hs@ 'ManageHook'.
dynamicMasterHook :: ManageHook
dynamicMasterHook = ask >>= \w -> liftX $ do
  dh <- XS.get
  (Endo f)  <- runQuery (permanent dh) w
  ts <- mapM (\(q,a) -> runQuery q w >>= \x -> return (x,(q, a))) (transients dh)
  let (ts',nts) = partition fst ts
  gs <- mapM (flip runQuery w . snd . snd) ts'
  let (Endo g) = fromMaybe (Endo id) $ listToMaybe gs
  XS.put $ dh { transients = map snd nts }
  return $ Endo $ f . g

-- | Appends the given 'ManageHook' to the permanent dynamic 'ManageHook'.
addDynamicHook :: ManageHook -> X ()
addDynamicHook m = updateDynamicHook (<+> m)

-- | Modifies the permanent 'ManageHook' with an arbitrary function.
updateDynamicHook :: (ManageHook -> ManageHook) -> X ()
updateDynamicHook f = XS.modify $ \dh -> dh { permanent = f (permanent dh) }

-- | Creates a one-shot 'ManageHook'. Note that you have to specify the two
-- parts of the 'ManageHook' separately. Where you would usually write:
--
-- > className =? "example" --> doFloat
--
-- you must call 'oneShotHook' as
--
-- > oneShotHook dynHooksRef (className =? "example) doFloat
--
oneShotHook :: Query Bool -> ManageHook -> X ()
oneShotHook q a = XS.modify $ \dh -> dh { transients = (q,a):transients dh }
