module IndependentScreens where

-- for the screen stuff
import Control.Arrow hiding ((|||))
import Control.Monad
import Control.Monad.Instances
import Data.List
import Graphics.X11.Xinerama
import XMonad
import XMonad.StackSet hiding (workspaces)

type VirtualWorkspace  = String
type PhysicalWorkspace = String

marshall :: ScreenId -> VirtualWorkspace -> PhysicalWorkspace
marshall (S sc) vws = show sc ++ '_':vws

unmarshall :: PhysicalWorkspace -> (ScreenId, VirtualWorkspace)
unmarshall = ((S . read) *** drop 1) . break (=='_')

workspaces' :: XConfig l -> [VirtualWorkspace]
workspaces' = nub . map (snd . unmarshall) . workspaces

withScreens :: ScreenId -> [VirtualWorkspace] -> [PhysicalWorkspace]
withScreens n vws = [marshall sc pws | pws <- vws, sc <- [0..n-1]]

onScreen :: (VirtualWorkspace -> WindowSet -> a) -> (PhysicalWorkspace -> WindowSet -> a)
onScreen f vws = screen . current >>= f . flip marshall vws

countScreens :: (MonadIO m, Integral i) => m i
countScreens = liftM genericLength . liftIO $ openDisplay "" >>= getScreenInfo
