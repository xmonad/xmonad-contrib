{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, PatternGuards, DeriveDataTypeable, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.LayoutBuilderP
-- Copyright   :  (c) 2009 Anders Engstrom <ankaan@gmail.com>, 2011 Ilya Portnov <portnov84@rambler.ru>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ilya Portnov <portnov84@rambler.ru>
-- Stability   :  unstable
-- Portability :  unportable
--
-- DEPRECATED.  Use 'XMonad.Layout.LayoutBuilder' instead.
--
-----------------------------------------------------------------------------

module XMonad.Layout.LayoutBuilderP {-# DEPRECATED "Use XMonad.Layout.LayoutBuilder instead" #-} (
  LayoutP (..),
  layoutP, layoutAll,
  B.relBox, B.absBox,
  -- * Overloading ways to select windows
  -- $selectWin
  Predicate (..), Proxy(..),
  ) where

import Control.Monad
import Data.Maybe (isJust)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.WindowProperties

import qualified XMonad.Layout.LayoutBuilder as B

-- $selectWin
--
-- 'Predicate' exists because layouts are required to be serializable, and
-- "XMonad.Util.WindowProperties" is not sufficient (for example it does not
-- allow using regular expressions).
--
-- compare "XMonad.Util.Invisible"

-- | Type class for predicates. This enables us to manage not only Windows,
-- but any objects, for which instance Predicate is defined.
--
-- Another instance exists in XMonad.Util.WindowPropertiesRE in xmonad-extras
class Predicate p w where
  alwaysTrue :: Proxy w -> p         -- ^ A predicate that is always True.
  checkPredicate :: p -> w -> X Bool -- ^ Check if given object (window or smth else) matches that predicate

-- | Contains no actual data, but is needed to help select the correct instance
-- of 'Predicate'
data Proxy a = Proxy

-- | Data type for our layout.
data LayoutP p l1 l2 a =
    LayoutP (Maybe a) (Maybe a) p B.SubBox (Maybe B.SubBox) (l1 a) (Maybe (l2 a))
    deriving (Show,Read)

-- | Use the specified layout in the described area windows that match given predicate and send the rest of the windows to the next layout in the chain.
--   It is possible to supply an alternative area that will then be used instead, if there are no windows to send to the next layout.
{-# DEPRECATED layoutP "Use XMonad.Layout.LayoutBuilder.layoutP instead." #-}
layoutP :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a, LayoutClass l3 a, Predicate p a) =>
       p
    -> B.SubBox                       -- ^ The box to place the windows in
    -> Maybe B.SubBox                 -- ^ Possibly an alternative box that is used when this layout handles all windows that are left
    -> l1 a                         -- ^ The layout to use in the specified area
    -> LayoutP p l2 l3 a              -- ^ Where to send the remaining windows
    -> LayoutP p l1 (LayoutP p l2 l3) a -- ^ The resulting layout
layoutP prop box mbox sub next = LayoutP Nothing Nothing prop box mbox sub (Just next)

-- | Use the specified layout in the described area for all remaining windows.
{-# DEPRECATED layoutAll "Use XMonad.Layout.LayoutBuilder.layoutAll instead." #-}
layoutAll :: forall l1 p a. (Read a, Eq a, LayoutClass l1 a, Predicate p a) =>
       B.SubBox             -- ^ The box to place the windows in
    -> l1 a               -- ^ The layout to use in the specified area
    -> LayoutP p l1 Full a  -- ^ The resulting layout
layoutAll box sub =
  let a = alwaysTrue (Proxy :: Proxy a)
  in  LayoutP Nothing Nothing a box Nothing sub Nothing

instance (LayoutClass l1 w, LayoutClass l2 w, Predicate p w, Show w, Read w, Eq w, Typeable w, Show p, Typeable p) =>
    LayoutClass (LayoutP p l1 l2) w where

        -- | Update window locations.
        runLayout (W.Workspace _ (LayoutP subf nextf prop box mbox sub next) s) rect
            = do (subs,nexts,subf',nextf') <- splitStack s prop subf nextf
                 let selBox = if isJust nextf'
                                then box
                                else maybe box id mbox

                 (sublist,sub') <- handle sub subs $ calcArea selBox rect

                 (nextlist,next') <- case next of Nothing -> return ([],Nothing)
                                                  Just n -> do (res,l) <- handle n nexts rect
                                                               return (res,Just l)

                 return (sublist++nextlist, Just $ LayoutP subf' nextf' prop box mbox sub' next' )
              where
                  handle l s' r = do (res,ml) <- runLayout (W.Workspace "" l s') r
                                     l' <- return $ maybe l id ml
                                     return (res,l')

        -- |  Propagate messages.
        handleMessage l m
            | Just (IncMasterN _) <- fromMessage m = sendFocus l m
            | Just (Shrink) <- fromMessage m = sendFocus l m
            | Just (Expand) <- fromMessage m = sendFocus l m
            | otherwise = sendBoth l m

        -- |  Descriptive name for layout.
        description (LayoutP _ _ _ _ _ sub (Just next)) = "layoutP "++ description sub ++" "++ description next
        description (LayoutP _ _ _ _ _ sub Nothing)     = "layoutP "++ description sub


sendSub :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a, Predicate p a)
        => LayoutP p l1 l2 a -> SomeMessage -> X (Maybe (LayoutP p l1 l2 a))
sendSub (LayoutP subf nextf prop box mbox sub next) m =
    do sub' <- handleMessage sub m
       return $ if isJust sub'
                then Just $ LayoutP subf nextf prop box mbox (maybe sub id sub') next
                else Nothing

sendBoth :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a, Predicate p a)
         => LayoutP p l1 l2 a -> SomeMessage -> X (Maybe (LayoutP p l1 l2 a))
sendBoth l@(LayoutP _ _ _ _ _ _ Nothing) m = sendSub l m
sendBoth (LayoutP subf nextf prop box mbox sub (Just next)) m =
    do sub' <- handleMessage sub m
       next' <- handleMessage next m
       return $ if isJust sub' || isJust next'
                then Just $ LayoutP subf nextf prop box mbox (maybe sub id sub') (Just $ maybe next id next')
                else Nothing

sendNext :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a, Predicate p a)
         => LayoutP p l1 l2 a -> SomeMessage -> X (Maybe (LayoutP p l1 l2 a))
sendNext (LayoutP _ _ _ _ _ _ Nothing) _ = return Nothing
sendNext (LayoutP subf nextf prop box mbox sub (Just next)) m =
    do next' <- handleMessage next m
       return $ if isJust next'
                then Just $ LayoutP subf nextf prop box mbox sub next'
                else Nothing

sendFocus :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a, Predicate p a)
          => LayoutP p l1 l2 a -> SomeMessage -> X (Maybe (LayoutP p l1 l2 a))
sendFocus l@(LayoutP subf _ _ _ _ _ _) m = do foc <- isFocus subf
                                              if foc then sendSub l m
                                                     else sendNext l m

isFocus :: (Show a) => Maybe a -> X Bool
isFocus Nothing = return False
isFocus (Just w) = do ms <- (W.stack . W.workspace . W.current) <$> gets windowset
                      return $ maybe False (\s -> show w == (show $ W.focus s)) ms


-- | Split given list of objects (i.e. windows) using predicate.
splitBy :: (Predicate p w) => p -> [w] -> X ([w], [w])
splitBy prop ws = foldM step ([], []) ws
  where
    step (good, bad) w = do
      ok <- checkPredicate prop w
      return $ if ok
                then (w:good, bad)
                else (good,   w:bad)

splitStack :: (Predicate p w, Eq w) => Maybe (W.Stack w) -> p -> Maybe w -> Maybe w -> X (Maybe (W.Stack w),Maybe (W.Stack w),Maybe w,Maybe w)
splitStack Nothing _ _ _ = return (Nothing,Nothing,Nothing,Nothing)
splitStack (Just s) prop subf nextf = do
    let ws = W.integrate s
    (good, other) <- splitBy prop ws
    let subf'  = foc good subf
        nextf' = foc other nextf
    return ( differentiate' subf' good
           , differentiate' nextf' other
           , subf'
           , nextf'
           )
  where
    foc [] _ = Nothing
    foc l f = if W.focus s `elem` l
              then Just $ W.focus s
              else if maybe False (`elem` l) f
                   then f
                   else Just $ head l

calcArea :: B.SubBox -> Rectangle -> Rectangle
calcArea (B.SubBox xpos ypos width height) rect = Rectangle (rect_x rect + fromIntegral xpos') (rect_y rect + fromIntegral ypos') width' height'
    where
        xpos' = calc False xpos $ rect_width rect
        ypos' = calc False ypos $ rect_height rect
        width' = calc True width $ rect_width rect - xpos'
        height' = calc True height $ rect_height rect - ypos'

        calc zneg val tot = fromIntegral $ min (fromIntegral tot) $ max 0 $
            case val of B.Rel v -> floor $ v * fromIntegral tot
                        B.Abs v -> if v<0 || (zneg && v==0)
                                 then (fromIntegral tot)+v
                                 else v

differentiate' :: Eq q => Maybe q -> [q] -> Maybe (W.Stack q)
differentiate' _ [] = Nothing
differentiate' Nothing w = W.differentiate w
differentiate' (Just f) w
    | f `elem` w = Just $ W.Stack { W.focus = f
                                  , W.up    = reverse $ takeWhile (/=f) w
                                  , W.down  = tail $ dropWhile (/=f) w
                                  }
    | otherwise = W.differentiate w

instance Predicate Property Window where
  alwaysTrue _ = Const True
  checkPredicate = hasProperty
