module IndependentScreens where

marshall (S sc) ws = show sc ++ '_':ws
unmarshall         = ((S . read) *** drop 1) . break (=='_')
workspaces'        = nub . map (snd . unmarshall) . workspaces
withScreens n workspaces = [marshall sc ws | ws <- workspaces, sc <- [0..n-1]]
onScreen    f workspace  = screen . current >>= f . flip marshall workspace
countScreens = fmap genericLength $ openDisplay "" >>= getScreenInfo
