{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Prelude

import Data.Ord
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

----------------------------------------------------------------------------------------------------

class SizeSortable a where
  getSize :: a -> Int

  sumSizes :: [a] -> Int
  sumSizes = sum . map getSize

  sortIncrSize :: [a] -> [a]
  sortIncrSize = sortBy $ comparing getSize

  sortDecrSize :: [a] -> [a]
  sortDecrSize = sortBy $ flip $ comparing getSize
  
----------------------------------------------------------------------------------------------------

type Name = String
type Size = Int

type Item = (Name,Size)

data Bin = Bin {
  name :: Name,
  size :: Size,
  content :: Set Item 
  } deriving Eq
  
----------------------------------------------------------------------------------------------------

instance Show Bin where
  show (Bin name space content) = 
    name ++ " (" ++ show space ++ "/" ++ show (space + sumSizes (Set.toList content)) ++ ")\n  ["
    ++ intercalate "," (map show $ Set.elems content) ++ "]"
  
instance SizeSortable Bin where
  getSize = size
  
instance SizeSortable Item where
  getSize = snd

----------------------------------------------------------------------------------------------------

-- HELPER FUNCTIONS

-- add item to bin
addItem :: Item -> Bin -> Bin
addItem item@(_, itemSize) (Bin name binSpace items)
  | binSpace < itemSize = error "Cannot add item to bin, overflow!"
  | otherwise = Bin name (binSpace - itemSize) (Set.insert item items)

-- find out total waste
totalWaste :: [Bin] -> Size
totalWaste bins = sumSizes $ filter (not . Set.null . content) bins

-- get set of bins with minimum waste from list of bin sets
minWaste :: [[Bin]] -> [Bin]
minWaste bss  | null nonnull = [] -- to avoid `head []` exception
              | otherwise = head stuff
  where
    nonnull = filter (not . null) bss
    stuff = sortBy (comparing totalWaste) nonnull

----------------------------------------------------------------------------------------------------

-- CONTRACT
pack :: [Item] -> [Bin] -> [Bin]
-- PURPOSE
-- Given a set of empty bins with fixed sizes and a set of items, return (one of the)
-- optimal solution(s) where the least space is wasted in used bins.
-- The basic idea is similar to BFD. Additionally, at each level of recursion we filter out alternatives with
-- the same total waste and keep only the best mean waste at each recursion branch.
-- Mean waste is the sum of square roots of waste in each used bin.

-- TO DO:
--    * optimal algorithm: http://aaaipress.org/Papers/AAAI/2002/AAAI02-110.pdf
--      Korf assumes a uniform bin size in his bin-completion algorithm though.
--      The general idea is similar to Best-Fit-Decreasing (BFD). But instead of always
--      taking the biggest remaining item, for each recursion step it is evaluated which
--      so called "feasible set" of remaining items (and if I understood correctly, this includes
--      all the items which are alone in some bin) "dominates" the other feasible sets and then adding
--      each undominated set to this bin in a separate recursion branch. A feasible set of items can be
--      placed in the bin worked on. Set A dominates set B if all items from B can be placed into bins from A
--      (implying to use items from A as bins). This means each creation of undominated sets is another
--      bin-packing problem and is solved recursively by Korf.
--      It seems that this step uses most of the total time of the algorithm and might be somewhat
--      improved by better implementation.
--      Each recursion branch is checked against a lower bound on numbers of bins used and discarded if appropriate.
--      I'm not sure how to convert this figure to varying bin sizes and the lower bound of wasted space.
--      As an optimization, the first run is BFD and the bin-completion algorithm is then fed with its
--      results to reduce the search space and discard branches early on.
--    * display results more prettily

-- EXAMPLES
-- pack [("TooBigItem",100)] [("TooSmallBin",50,[])] : []
-- pack [("Item1",50),("Item2",50)] [("Bin",100,[])] : [("Bin",0,[("Item1",50),("Item2",50)])]

-- DEFINITION
pack [] bins = bins
-- take the result with the smallest space wasted
pack items@(_:_) bins = minWaste $ map (uncurry pack) step
  where
    -- brute force each combination
    step = [ (filter (/= i) items, addItem i b : filter (/= b) bins) 
              | i <- items, b <- bins, getSize i <= getSize b ]


----------------------------------------------------------------------------------------------------

{- [BFD]
Sorts the elements in decreasing order of size and puts each element into the fullest
bin in which is fits. It can be implemented by keeping the bins sorted in increasing order 
of their remaining capacity, and placing each element into the first bin in which it fits.

11 -> [9, 10, 11, 12, 9001] = [9, 10, 0, 12, 9001]
insert element (naive: recursively) and return resulting list.
the remaining size of the in obviousely decreased -> it should be moved to the front
-}

-- | Implementation of best-fit-decreasing as described in the paper by Korf.
bfd :: [Bin] -> [Item] -> [Bin]
bfd bins = foldr bfdSingle bins 

bfdSingle :: Item -> [Bin] -> [Bin]
bfdSingle _ [] = error "Not enough bins!"
bfdSingle item@(_, itemSize) (bin@(Bin _ binSize _) : bins)
  | itemSize <= binSize = addItem item bin : bins 
  | otherwise = let
      (res@(Bin _ resSize _) : ress) = bfdSingle item bins
      in if resSize < binSize 
      then res : bin : ress
      else bin : res : ress

----------------------------------------------------------------------------------------------------

{-
-- Check if given system could be solvable
solvable :: [Item] -> [Bin] -> Maybe Size
-- Return Nothing when definitely not solvable, otherwise returns sum of all bin sizes
solvable items bins 
  | sumItemSize items > sumBinSpace bins || sizeOf (last (sortItems items)) > spaceOf(last (sortBins bins)) = Nothing
  | otherwise = Just $ sumBinSpace bins
-}

-- TEST DATA
test_bin :: [Bin]
test_bin = sortIncrSize [
  Bin "A" 38 Set.empty,
  Bin "C" 50 Set.empty,
  Bin "B" 110 Set.empty
  ]

test_item :: [Item]
test_item = sortDecrSize [
  ("x",5),
  ("y",6),
  ("z",4),
  ("a",4),
  ("b",4),
  ("j",5)]

main :: IO ()
main =  do
      let res = bfd test_bin test_item 
      mapM_ print res
      putStrLn ""
      putStrLn $ "Total waste: " ++ show (totalWaste res)
      
      
      
      
