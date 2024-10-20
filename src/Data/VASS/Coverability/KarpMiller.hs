{-# LANGUAGE TypeApplications, OverloadedLists #-}
{-| The standard Karp-Miller algorithm. This was originally implemented
    as a portion of the Kosaraju reachability algorithm for Petri Nets.
-}
module Data.VASS.Coverability.KarpMiller (karpMiller, karpMillerTree, karpMiller') where

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Graph as Tree
import Data.Graph (Tree(..))
import qualified Data.List as List
import Data.Maybe

import Text.Pretty.Simple

import Data.VASS.Coverability.KarpMiller.Shared
import Data.VASS.Coverability.KarpMiller.ExtendedNaturals

import Data.VASS.Coverability
import Data.VASS
import Data.Bifunctor (first)
import Data.Foldable (find)
import Data.Tree (flatten)
import Data.Ord (comparing)

{-| This is the standard entrypoint for the checker as used by Duvet and other
    tools. You can evaluate it directly too if you want.

    For more direct access to the underlying tree, see 'karpMillerTree'.
-}
karpMiller :: Int -> CovChecker
karpMiller depth (CovProblem vass initial target) =
    let
        tree = karpMillerTree depth initial vass
    in return $ if fmap fst tree `contains` extend target
            then Safe
            else Unsafe

karpMiller' :: Int -> CovProblem -> (CovResult, [(ExtConf, Vector Transition)])
karpMiller' depth (CovProblem vass initial target) =
    let
        tree = karpMillerTree depth initial vass
        -- transitions = snd <$> find (\t' -> extend target <= fst t') tree
        transitions =
          filter (\t' -> extend target <= fst t')
          $ flatten tree
    in if fmap fst tree `contains` extend target
            then (Safe, transitions)
            else (Unsafe, mempty)


{- | Construct the Karp-Miller Tree which represents the coverability set.
    This is an implementation of the standard algorithm, and does not include
    any accelerations. It forms a useful baseline to confirm that accelerations
    are giving the correct results.

    The original KM definition performed a breadth-first construction; as there is
    no pruning, the tree is always maximal and therefore the order of construction
    is not relevant. Depth-first search is more natural in a recursive format.
-}
karpMillerTree :: Int -> Conf -> VASS -> KarpMillerTree
karpMillerTree maxDepth initial VASS{..} = let

    -- | All VASS states which can be reached by one transition from our
    -- current configuration.
    reachableFrom :: ExtConf -> Vector (ExtConf, Transition)
    reachableFrom conf@(Configuration state vec) =
      let trans' = transitions !@ state
          trans = Vector.filter (`activeFrom` conf) trans'
      in fmap (\t -> (conf |> t, t)) trans
        -- [ (conf |> trans, trans)
        -- | trans <- Vector.toList $ transitions !@ state
        -- , trans `activeFrom` conf
        -- ]
    {-# INLINE reachableFrom #-}

    -- | Our acceleration step
    -- we can jump to omega in any places which strictly increase.
    addOmegas :: ExtConf -> ExtConf -> ExtConf
    addOmegas (Configuration s vec) (Configuration s' ancestor)
        | s /= s'          = Configuration s vec
        | ancestor <= vec  = Configuration s (Vector.zipWith makeOmega vec ancestor)
        | otherwise        = Configuration s vec
        where makeOmega v a
                | v > a    = Omega
                | otherwise = v

    -- | Recursive depth-first construction of the KM tree.
    -- When altitude = 0, the tree ends
    treeRec :: Int -> Vector ExtConf -> (ExtConf, Vector Transition) -> Maybe KarpMillerTree
    treeRec altitude ancestors (current@(Configuration state vec), ts) = let

        current' =
          List.foldl'
            addOmegas
            current
            ancestors
        reachables :: Vector (ExtConf, Transition)
        reachables = reachableFrom current'

        -- If the node has previously been seen, we will not build it
        in
            if altitude == 0 || ancestors `contains` current then Nothing
            else
            Just
              . Node (current', ts)
              . Vector.toList
              $ Vector.mapMaybe
                (\(l, t) ->
                   treeRec
                     (altitude - 1)
                     (Vector.cons current' ancestors)
                     (l, Vector.snoc ts t))
                reachables

    in fromJust $ treeRec maxDepth mempty (extend initial, mempty)
