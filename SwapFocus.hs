module XMonadContrib.SwapFocus ( swapFocus ) where

-- swaps focus with last-focussed window.

-- To use:
-- import XMonadContrib.SwapFocus ( swapFocus )

--    , ((modMask .|. shiftMask, xK_Tab), swapFocus)

import Control.Monad.State

import Operations ( refresh )
import XMonad ( X, WindowSet, windowset )
import StackSet ( StackSet, peekStack, popFocus, pushFocus, current )

sf :: (Integral i, Integral j, Ord a) => StackSet i j a -> Maybe (StackSet i j a)
sf w = do let i = current w
          f1 <- peekStack i w
          f2 <- peekStack i $ popFocus i f1 w
          return $ pushFocus i f2 $ pushFocus i f1 w

swapFocus :: X ()
swapFocus = smartwindows sf

-- | smartwindows. Modify the current window list with a pure function, and only refresh if necesary
smartwindows :: (WindowSet -> Maybe WindowSet) -> X ()
smartwindows f = do w <- gets windowset
                    case (f w) of Just f' -> do modify $ \s -> s { windowset = f' }
                                                refresh
                                  Nothing -> return ()
