-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DynamicHooks
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
  initDynamicHooks
  ,dynamicMasterHook
  ,addDynamicHook
  ,updateDynamicHook
  ,oneShotHook
  ) where

import XMonad
import System.IO

import Data.List
import Data.Maybe (listToMaybe)
import Data.Monoid
import Data.IORef

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
-- First, you must execute 'initDynamicHooks' from 'main' in your @xmonad.hs@:
--
-- > dynHooksRef <- initDynamicHooks
--
-- and then pass this value to the other functions in this module.
--
-- You also need to add the base 'ManageHook':
--
-- > xmonad { manageHook = myManageHook <+> dynamicMasterHook dynHooksRef }
--
-- You must include this @dynHooksRef@ value when using the functions in this
-- module:
--
-- > xmonad { keys = myKeys `Data.Map.union` Data.Map.fromList 
-- >                   [((modMask conf, xK_i), oneShotHook dynHooksRef 
-- >                    "FFlaunchHook" (className =? "firefox") (doShift "3") 
-- >                    >> spawn "firefox")
-- >                   ,((modMask conf, xK_u), addDynamicHook dynHooksRef 
-- >                     (className =? "example" --> doFloat))
-- >                   ,((modMask conf, xK_y), updatePermanentHook dynHooksRef
-- >                     (const idHook))) ]  -- resets the permanent hook.
--

data DynamicHooks = DynamicHooks
    { transients :: [(Query Bool, ManageHook)]
    , permanent  :: ManageHook }
      

-- | Creates the 'IORef' that stores the dynamically created 'ManageHook's.
initDynamicHooks :: IO (IORef DynamicHooks)
initDynamicHooks = newIORef (DynamicHooks { transients = [],
                                            permanent = idHook })


-- this hook is always executed, and the IORef's contents checked.
-- note that transient hooks are run second, therefore taking precedence
-- over permanent ones on matters such as which workspace to shift to.
-- doFloat and doIgnore are idempotent.
-- | Master 'ManageHook' that must be in your @xmonad.hs@ 'ManageHook'.
dynamicMasterHook :: IORef DynamicHooks -> ManageHook
dynamicMasterHook ref = return True --> 
                  (ask >>= \w -> liftX (do
  dh <- io $ readIORef ref
  (Endo f)  <- runQuery (permanent dh) w
  ts <- mapM (\(q,a) -> runQuery q w >>= \x -> return (x,(q, a))) (transients dh)
  let (ts',nts) = partition fst ts
  gs <- mapM (flip runQuery w . snd . snd) ts'
  let (Endo g) = maybe (Endo id) id $ listToMaybe gs
  io $ writeIORef ref $ dh { transients = map snd nts }
  return $ Endo $ f . g
                                       ))

-- | Appends the given 'ManageHook' to the permanent dynamic 'ManageHook'.
addDynamicHook :: IORef DynamicHooks -> ManageHook -> X ()
addDynamicHook ref m = updateDynamicHook ref (<+> m)


-- | Modifies the permanent 'ManageHook' with an arbitrary function.
updateDynamicHook :: IORef DynamicHooks -> (ManageHook -> ManageHook) -> X ()
updateDynamicHook ref f = 
    io $ modifyIORef ref $ \dh -> dh { permanent = f (permanent dh) }


-- | Creates a one-shot 'ManageHook'. Note that you have to specify the two
-- parts of the 'ManageHook' separately. Where you would usually write:
--
-- > className =? "example" --> doFloat
--
-- you must call 'oneShotHook' as 
--
-- > oneShotHook dynHooksRef (className =? "example) doFloat
-- 
oneShotHook :: IORef DynamicHooks -> Query Bool -> ManageHook -> X ()
oneShotHook ref q a =
  io $ modifyIORef ref
         $ \dh -> dh { transients = (q,a):(transients dh) }




