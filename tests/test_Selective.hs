{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

import XMonad.Layout.Selective
import XMonad.StackSet hiding (focusUp, focusDown)
import Control.Applicative ((<$>))
import Test.QuickCheck
import Control.Arrow (second)

instance Arbitrary (Stack Int) where
    arbitrary = do
                    xs <- arbNat
                    ys <- arbNat
                    return $ Stack { up=[xs-1,xs-2..0], focus=xs, down=[xs+1..xs+ys] }
    coarbitrary = undefined

instance Arbitrary Selection where
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
    where
        sel' = update sel stk

-- update normalizes selections (is idempotent)
prop_update_idem sel (stk :: Stack Int) = sel' == update sel' stk
    where
        sel' = update sel stk

-- select selects the master pane
prop_select_master sel (stk :: Stack Int) = 
    take (nMaster sel) (integrate stk) == take (nMaster sel) (integrate $ select sel stk)

-- the focus should always be selected in normalized selections
prop_select_focus sel (stk :: Stack Int) = focus stk == (focus $ select sel' stk)
    where
        sel' = update sel stk

-- select doesn't change order (or duplicate elements)
-- relies on the Arbitrary instance for Stack Int generating increasing stacks
prop_select_increasing sel (stk :: Stack Int) =
    let res = integrate $ select sel stk
     in and . zipWith (<) res $ tail res

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

upSel sel stk = let sel' = update sel stk in (sel', select sel' stk)

focusUp stk = stk { up=tail (up stk), focus=head (up stk), down=focus stk:down stk }
focusDown stk = stk { down=tail (down stk), focus=head (down stk), up=focus stk:up stk }
