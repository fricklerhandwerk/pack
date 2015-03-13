
import Data.Ord
import Data.List

type Name = String
type Size = Int
type Item = (Name,Size)
type Bin = (Name,Size,[Item])


-- CONTRACT
--pack :: [Item] -> [Bin] -> [Bin]

-- PURPOSE
-- Given a set of empty bins with fixed sizes and a set of items, return (one of the)
-- optimal solution(s) where the least space is wasted in used bins.
-- The basic idea is that at each level of recursion we filter out alternatives with
-- the same total waste and keep only the best waste at each recursion branch.
-- We also always take the biggest remaining item and only recurse through options where it fits

-- TO DO:
--		* optimal algorithm: http://aaaipress.org/Papers/AAAI/2002/AAAI02-110.pdf
--			Korf assumes a uniform bin size in his bin-completion algorithm though.
--			The general idea is similar to Best-Fit-Decreasing (BFD). But instead of always
--			taking the biggest remaining item, for each recursion step it is evaluated which
--			so called "feasible set" of remaining items (and if I understood correctly, this includes
--			all the items which are alone in some bin) "dominates" the other feasible sets and then adding
--			each undominated set to this bin in a separate recursion branch. A feasible set of items can be
--			placed in the bin worked on. Set A dominates set B if all items from B can be placed into bins from A
--			(implying to use items from A as bins).	This means each creation of undominated sets is another
--			bin-packing problem and is solved recursively by Korf.
--			It seems that this step uses most of the total time of the algorithm and might be somewhat
--			improved by better implementation.
--			Each recursion branch is checked against a lower bound on numbers of bins used and discarded if appropriate.
--			I'm not sure how to convert this figure to varying bin sizes and the lower bound of wasted space.
--			As an optimization, the first run is BFD and the bin-completion algorithm is then fed with its
--			results to reduce the search space and discard branches early on.
--		* display results more prettily

-- EXAMPLES
-- pack [("TooBigItem",100)] [("TooSmallBin",50,[])] : []
-- pack [("Item1",50),("Item2",50)] [("Bin",100,[])] : [("Bin",0,[("Item1",50),("Item2",50)])]

-- DEFINITION

-- filter redundancy before and after recursing, but only by waste
pack [] bins = bins
-- assume items are sorted descending
pack (i:is) bins = head $ nubBy compareWaste $ sortBy (comparing meanWaste) $ map (pack is) unique
	where
		-- produce a list of alternatives where the biggest remaining item fits in
		-- somewhere at the end of this line should be the place to find dominant feasible sets to branch on
		permutations = [ addItem i b : filter (/= b) bins | b <- bins, spaceOf b >= sizeOf i]
		-- filter variants with same waste
		unique = nubBy compareWaste permutations
		-- compare two alternatives by total waaste
		compareWaste x y = totalWaste x == totalWaste y-- && meanWaste x == meanWaste y


-- HELPER FUNCTIONS

-- get size of item
sizeOf :: Item -> Size
sizeOf = snd

-- get free space of bin
spaceOf :: Bin -> Size
spaceOf (_,space,_) = space

-- get name of bin
nameOf :: Bin -> Name
nameOf (name,_,_) = name

-- get items from bin
contentOf :: Bin -> [Item]
contentOf (_,_,items) = items

-- sum of bins' free space
sumBinSpace bins = sum [spaceOf a | a <- bins]
-- sum of items' sizes
sumItemSize items = sum [sizeOf a | a <- items]
-- sort bins by free space
sortBins bins = sortBy (comparing spaceOf) bins
-- sort items by size descending, then name ascending
-- assumes unique item names
sortItems items = sortBy compareItems items
	where
		compareItems (a1, b1) (a2, b2)
		  | b1 < b2 = GT
		  | b1 > b2 = LT
		  | b1 == b2 = compare a1 a2

-- add item to bin
addItem item (name,space,items) = (name,space - (sizeOf item),item:items)
-- find out total waste
totalWaste bins = sumBinSpace $ filter (not.null.contentOf) bins
-- calculate mean waste of a set of bins
meanWaste bins = sum wastes-- / (fromIntegral $ length wastes)
	where
		wastes = map (sqrt.fromIntegral.spaceOf) $ filter (not.null.contentOf) bins
{-
-- Check if given system could be solvable
solvable :: [Item] -> [Bin] -> Either Bool Size
-- Return `False` when definitely not solvable, otherwise returns sum of all bin sizes
solvable items bins | sumItemSize items > sumBinSpace bins || sizeOf (last (sortItems items)) > spaceOf(last (sortBins bins)) = Left False
					| otherwise = Right (sumBinSpace bins)
-}

-- SHOW FUNCTIONS

itemToString :: Item -> Name
itemToString (name,size) = name 
						++ " ("
						++ show size
						++ ")"

itemListToString :: [Item] -> Name
itemListToString (i:is) = itemToString i ++ next
	where
		next	| null is = ""
					| otherwise = " | " ++ itemListToString is

binToString :: Bin -> Name
binToString bin@(name,size,items) =
  name
	++ " "
	++ show (spaceOf bin)
	++ "/"
	++ show (spaceOf bin + sumItemSize items)
	++ itemString
	++ "\n"
  	where
  		itemString	| null items = ""
  								| otherwise = "\n   " ++ itemListToString items

binListToString :: [Bin] -> Name
binListToString (b:bs) = binToString b ++ next
	where
		next	| null bs = "\n"
				| otherwise = "\n" ++ binListToString bs

alternativesToString :: [[Bin]] -> Name
alternativesToString (bs:bss) = binListToString bs ++ next
	where
		next	| null bss = "\n"
					| otherwise = "----\n\n" ++ alternativesToString bss
-- TEST DATA
bin = sortBins [
	("A",30,[]),
	("C",15,[]),
	("D",100,[]),
	("B",110,[])] :: [Bin]

item = sortItems [
	("x",5),
	("y",6),
	("z",1),
	("a",2),
	("b",3),
	("k",4),
	("l",7),
	("j",8)] :: [Item]

main =	do
			let res = pack item bin
			putStrLn $ show res
			putStr "\n"
			putStrLn $ "Total Waste: " ++ (show $ totalWaste res)
