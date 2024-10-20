{-# LANGUAGE OverloadedLists #-}
{-| Tools for drawing the trees constructed by the Karp-Miller algorithm.

    This will output SVG diagrams using the Diagrams package.

    Note: You may find that, on large outputs, this takes a very, very long time!
-}
{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Data.VASS.Coverability.KarpMiller.Render where

import Data.VASS
import Data.VASS.Coverability

import Data.VASS.Coverability.KarpMiller.ExtendedNaturals
import Data.VASS.Coverability.KarpMiller.Shared
import Data.VASS.Coverability.KarpMiller
-- import Data.VASS.Coverability.KarpMillerFast

import Diagrams.Prelude hiding (render)
import Data.Tree (Tree(..))
import qualified Data.Vector as Vector
import Diagrams.Backend.SVG
import Data.Function ((&))
import Graphics.SVGFonts
import Diagrams.Backend.Rasterific

import Diagrams.TwoD.Layout.Tree
import Data.VASS.Read (readAny)
-- import Data.VASS.Coverability.KarpMillerFast (constructKarpMillerTree)
import Data.Bifunctor (second)


-- | Given a VAS and a filepath, render the Karp-Miller tree as an SVG
-- and save it to that file.
renderSpec :: CovProblem -> FilePath -> IO ()
renderSpec (CovProblem v i t) path = renderKM path $ karpMillerTree maxBound i v

-- -- | Given a file path, output the generated Karp-Miller tree as an SVG.
-- render :: FilePath -> KarpMillerTree -> IO ()
-- render path kmtree =
--   -- renderRasterific path size (scale sf diagram)
--   renderSVG path size (scale sf diagram)
--     where hsep = let Node a _ = kmtree in fromIntegral (dim (fst a) + 4 + 10)
--           diagram = renderTree
--             (text)
--             (arrowBetween' (with & shaftStyle %~ lw (local 0.1)
--                                  & headLength .~ local 1
--                                  & headGap .~ local 1
--                                  & tailGap .~ local 1))
--             (symmLayout' (with & slHSep .~ hsep & slVSep .~ 10) (toCTree kmtree))
--             # centerXY # pad 1.1

--           -- text' t = stroke (textSVG t 1)
--           size :: SizeSpec V2 Double
--           size = dims $ r2 (1600,1600)

--           sf :: Double
--           sf = 1

-- | Given a file path, output the generated Karp-Miller tree as an SVG.
render' :: FilePath -> (a -> Tree String) -> (a -> Double) -> a -> IO ()
render' path toCTree fHSep kmtree =
  -- renderRasterific path size (scale sf diagram)
  renderSVG path size (scale sf diagram)
    where hsep = fHSep kmtree -- let Node a _ = kmtree in fromIntegral (dim (fst a) + 4 + 10)
          diagram = renderTree
            (text)
            (arrowBetween' (with & shaftStyle %~ lw (local 0.1)
                                 & headLength .~ local 1
                                 & headGap .~ local 1
                                 & tailGap .~ local 1))
            (symmLayout' (with & slHSep .~ hsep & slVSep .~ 100) (toCTree kmtree))
            -- # centerXY # pad 1.1

          -- text' t = stroke (textSVG t 1)
          size :: SizeSpec V2 Double
          size = dims $ r2 (3200,3200)

          sf :: Double
          sf = 100

renderKM :: FilePath -> KarpMillerTree -> IO ()
renderKM path = render' path toCTree (\t -> let Node a _ = t in fromIntegral (dim (fst a) + 4 + 10))

-- renderKMF :: FilePath -> KMF.KMTree -> IO ()
-- renderKMF path = render' path toCTree'
--   (\t ->
--      case t of
--        KMF.DeadEnd (a, _) -> fromIntegral (dim a + 4 + 20)
--        KMF.Node (a, _) _ -> fromIntegral (dim a + 4 + 20))

{-| Helper function for rendering the parts of the Karp-Miller trees into strings.

    Nodes with no children (ie dead ends) are marked with a box.
-}
toCTree :: KarpMillerTree -> Tree String
toCTree (Node (a, ts) []) = Node (show a++", " ++ showTf ts ++ " ■") []
toCTree (Node (a, ts) cs) = Node (show a ++ ", " ++ showTf ts) $ fmap toCTree cs

-- toCTree' :: KMF.KMTree -> Tree String
-- toCTree' (KMF.Node (a, ts) []) = Node (show a++", \n" ++ showTf ts ++ " ■") []
-- toCTree' (KMF.Node (a, ts) cs) =
--   Node
--     (show a ++ ", \n" ++ showTf ts)
--     $ Vector.toList $ fmap toCTree' cs
-- toCTree' (KMF.DeadEnd (a, ts)) = Node (show a ++ ", \n" ++ showTf ts ++ " DEAD") []

showT :: Transition -> String
showT = show . name

showTf :: (Show (f (Data.VASS.Name Transition)), Functor f) => f Transition -> String
showTf = show . fmap name

{-
>>> CovProblem system initial target <- readAny "test/model-simple.spec"
>>> tree = karpMillerTree initial system
>>> render "~/Projects/Haskell/karp-miller/test-diagram.svg" tree
-}

-- test file = do
--   cov@(CovProblem system initial target) <- readAny $ "test/" <> file
--   -- let tree = karpMillerTree initial system
--   let tree = constructKarpMillerTree initial system
--       res = second (fmap (fmap name)) $ KMF.karpMillerF cov
--   print res
--   renderKMF "test-diagram.svg" tree
--   -- print $ show tree

testOld file = do
  cov@(CovProblem system initial target) <- readAny $ "test/" <> file
  let -- tree = karpMillerTree 4 initial system
      -- res = second (fmap name) $ karpMiller' 4 cov
      res = second (fmap (fmap (fmap name))) $ karpMiller' 4 cov
  print system
  print res
  -- renderKM "test-diagram.svg" tree
  -- print $ show tree
