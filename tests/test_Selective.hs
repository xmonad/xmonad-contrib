{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

-- Tests for limitSelect-related code in L.LimitWindows.
-- To run these tests, export (select,update,Selection(..),updateAndSelect) from
-- L.LimitWindows.

import XMonad.Layout.LimitWindows
import XMonad.StackSet hiding (focusUp, focusDown, filter)
import Control.Applicative ((<$>))
import Test.QuickCheck
import Control.Arrow (second)

instance Arbitrary (Stack Int) where
    arbitrary = do
                    xs <- arbNat
                    ys <- arbNat
                    return $ Stack { up=[xs-1,xs-2..0], focus=xs, down=[xs+1..xs+ys] }
    coarbitrary = undefined

instance Arbitrary (Selection a) where
    arbitrary = do
                    nm <- arbNat
                    st <- arbNat
                    nr <- arbPos
                    return $ Sel nm (st+nm) nr
    coarbitrary = undefined

arbNat = abs <$> arbitrary
arbPos = (+1) . abs <$> arbitrary

-- as many windows as possible should be selected 
-- (when the selection is normalized)
prop_select_length sel (stk :: Stack Int) =
    (length . integrate $ select sel' stk) == ((nMaster sel' + nRest sel') `min` length (integrate stk))
    where sel' = update sel stk

-- update normalizes selections (is idempotent)
prop_update_idem sel (stk :: Stack Int) = sel' == update sel' stk
    where sel' = update sel stk

-- select selects the master pane
prop_select_master sel (stk :: Stack Int) = 
    take (nMaster sel) (integrate stk) == take (nMaster sel) (integrate $ select sel stk)

-- the focus should always be selected in normalized selections
prop_select_focus sel (stk :: Stack Int) = focus stk == (focus $ select sel' stk)
    where sel' = update sel stk

-- select doesn't change order (or duplicate elements)
-- relies on the Arbitrary instance for Stack Int generating increasing stacks
prop_select_increasing sel (stk :: Stack Int) =
    let res = integrate $ select sel stk
     in and . zipWith (<) res $ tail res

-- selection has the form [0..l] ++ [m..n]
-- relies on the Arbitrary instance for Stack Int generating stacks like [0..k]
prop_select_two_consec sel (stk :: Stack Int) =
    let wins = integrate $ select sel stk
     in (length . filter not . zipWith ((==) . (+1)) wins $ tail wins) <= 1

-- update preserves invariants on selections
prop_update_nm sel (stk :: Stack Int) = nMaster (update sel stk) >= 0
prop_update_start sel (stk :: Stack Int) = nMaster sel' <= start sel'
    where sel' = update sel stk
prop_update_nr sel (stk :: Stack Int) = nRest (update sel stk) >= 0

-- moving the focus to a window that's already selected doesn't change the selection
prop_update_focus_up sel (stk :: Stack Int) x' =
    (length (up stk) >= x) && ((up stk !! (x-1)) `elem` integrate stk') ==> 
        sel' == update sel' (iterate focusUp stk !! x)
    where
        x = 1 + abs x'
        sel' = update sel stk
        stk' = select sel' stk

prop_update_focus_down sel (stk :: Stack Int) x' =
    (length (down stk) >= x) && ((down stk !! (x-1)) `elem` integrate stk') ==> 
        sel' == update sel' (iterate focusDown stk !! x)
    where
        x = 1 + abs x'
        sel' = update sel stk
        stk' = select sel' stk

focusUp stk = stk { up=tail (up stk), focus=head (up stk), down=focus stk:down stk }
focusDown stk = stk { down=tail (down stk), focus=head (down stk), up=focus stk:up stk }
