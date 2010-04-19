{-# LANGUAGE ViewPatterns #-}
{- | generate another Main from all modules in the current directory,
extracting all functions with @prop_@.

Usage (your QuickCheck-1 version may vary):

> ln -s ../../xmonad/tests/Properties.hs .
> runghc genMain.hs > Main.hs
> ghc -DTESTING -i.. -i. -package QuickCheck-1.2.0.0 Main.hs -e main

-}
module Main where

import Control.Monad.List
import Data.Char
import Data.IORef
import Data.List
import qualified Data.Set as S
import System.Directory
import System.FilePath
import Text.PrettyPrint.HughesPJ

main = do
    imports <- newIORef S.empty
    props <- runListT $ do
        f @ ((isUpper -> True) : (takeExtension -> ".hs"))
            <- ListT (getDirectoryContents ".")
        guard $ f `notElem` ["Main.hs", "Common.hs", "Properties.hs"]
        let b = takeBaseName f
        nesting <- io $ newIORef 0
        decl : _ <- ListT $ (map words . lines) `fmap` readFile f
        case decl of
            "{-" -> io $ modifyIORef nesting succ
            "-}" -> io $ modifyIORef nesting pred
            _ -> return ()
        0 <- io $ readIORef nesting
        guard $ "prop_" `isPrefixOf` decl
        io $ modifyIORef imports (S.insert b)
        return (b ++ "." ++ decl)
    imports <- S.toList `fmap` readIORef imports
    print $ genModule imports props

genModule :: [String] -> [String] -> Doc
genModule imports props = vcat [header,imports', main ]
    where
        header = text "module Main where"
        imports' = text "import Test.QuickCheck"
                $$ vcat [ text "import qualified" <+> text im | im <- imports ]
        props' = vcat [ text "quickCheck" <+> text p | p <- props ]
        main = hang (text "main = do") 4 props'

io x = liftIO x
