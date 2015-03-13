import copy

class Bin(object):
	def __init__(self,name,size):
		self.items = []
		self.name = name
		self.space = size
		self.size = size

	def append(self, item):
		self.items.append(item)
		self.space -= item.size

	def used(self):
		if len(self.items) > 0:
			return True
		else:
			return False

	def __cmp__(self,other):
		if self.space > other.space:
			return 1
		elif self.space < other.space:
			return -1
		else:
			return 0

	def __str__(self):
		return '%s [%d/%d] \n%s' % (self.name, self.space, self.size, str(self.items))

	def __repr__(self):
		return str(self) + '\n'

class Item(object):
	def __init__(self,name,size):
		self.name = name
		self.size = size

	def __cmp__(self,other):
		if self.size > other.size:
			return 1
		elif self.size < other.size:
			return -1
		else:
			return 0

	def __str__(self):
		return '%s [%d]' % (self.name,self.size)

	def __repr__(self):
		return '\t' + str(self) + '\n'

bestBins = 0
bestWaste = 0
iterations = 0

# check input values for solvability
def solvable(I,B):
	sumItemSize = 0
	sumBinSpace = 0

	maxItem = 0
	maxBin = 0

	for i in I:
		sumItemSize += i.size
		if i.size > maxItem:
			maxItem = i.size
	for b in B:
		sumBinSpace += b.size
		if b.size > maxBin:
			maxBin = b.size

	# while we're at it, set upper bound for
	global bestBins
	global bestWaste
	# minimum total number of bins used
	bestBins = len(B)
	# minimum total waste
	bestWaste = sumBinSpace

	if sumItemSize > sumBinSpace or maxItem > maxBin:
		return False
	else:
		return True

# brute force approach to bin-packing
def pack(I,B):

	global bestBins
	global bestWaste
	global iterations

	print '\r %d' % iterations,

	## BASE CASE

	# find out current total waste
	currentWaste = 0
	for b in B:
		if b.used():
			currentWaste += b.space

	# find out total remaining item size
	rest = 0
	for i in I:
		rest += i.size

	# stop if adding all the items to the current bins will not improve the best result
	if currentWaste - rest >= bestWaste:
		return

	# stop when all items are exhausted
	if len(I) == 0:

		if currentWaste < bestWaste:
			# find out total size of bins used
			currentBins = 0
			currentSize = 0
			# all used bins with contents
			binsUsed = []

			for b in B:
				if b.used():
					binsUsed.append(b)
					currentBins += 1
					currentSize += b.size

			print "\rResult found using %d bins (%d/%d wasted) after %d iterations:\n%s" % (currentBins,currentWaste,currentSize,iterations,binsUsed)
			bestBins = currentBins
			bestWaste = currentWaste
		return

	## RECURSION
	B = sorted(B)#[::-1]
	for item in I:
		iterations += 1
		# check each combination
		for bin in B:
			if bin.space - item.size >= 0:
				iterations += 1
				# create copy of B where current bin contains current item
				b1 = copy.deepcopy(bin) # deepcopy is very important here!
				b1.append(item)

				B1 = copy.deepcopy(B)
				B1.remove(bin)
				B1.append(b1)

				# create copy of item list without current item
				I1 = copy.deepcopy(I)
				I1.remove(item)

				# go down recursion
				pack(I1,B1)
		# stop if item fits in none of the bins
		# NOTE: this is really some sneaky language construct!
		else:
			return

# hard coded tuple representation of stuff
binTuples = [
	("A",30),
	("C",15),
	("D",100),
	("B",110)]

itemTuples = [
	("x",5),
	("y",6),
	("z",1),
	("a",2),
	("b",3),
	("k",4),
	("l",7),
	("j",8)]

# make lists of objects, both sorted descending
bins = sorted([Bin(x[0],x[1]) for x in binTuples])#[::-1]
items = sorted([Item(x[0],x[1]) for x in itemTuples])[::-1]


if solvable(items, bins):
	pack(items,bins)
else:
	print "Problem set not solvable!"
