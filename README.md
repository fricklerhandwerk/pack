# pack
Solver for a variant of the bin packing problem

# The Problem
The [standard bin packing problem](https://en.wikipedia.org/wiki/Bin_packing_problem) consists of a finite set of variable-sized items and arbitrary number of fix-sized bins. The goal is to put all items into as few bins as possible.

**In this case** we have the same kind of items, but there is a fixed number of bins with varying size. The goal is to place the items in a subset of the bins, optimizing for wasted bin space.

### Example
```
Items: 1,2,3,4,5  
Bins: (A,2)  (B,4)  (C,10)  
Result: (A,0,[2])  (B,0,[4])  (C,1,[1,3,5])
```

The practical idea behind it is to optimally store big chunks of data on a given set of different hard disks.

# Status

At the moment it is just an experiment to implement the `best fit decreasing` in Python and Haskell.

The Python version works reliably and first prints the `BDF` result, then continues to work through the complete search space for better solutions. Currently the problem is hard coded in the source. Some work could be done to make the thing more useful and process input from `stdin` or a file, such that the program recieves a problem set to work on. The discarding of search branches also needs a revision, maybe there is some potential for optimization.

The Haskell implementation is a smoking wreck and needs lots of cleanup. There is a `BDF` function (supplied by Alex, thanks!) and another approach that tries to cover more search space, which is relatively slow but computes small problem sets properly. Worst problem is that there are two unmerged versions from Alex and me.

Currently not all potentially meaningful search branches are evaluated. Therefore most important would be to provide a function that determines `undominated feasible sets` more or less efficiently to implement the optimal algorithm proposed in [Korf's paper](http://aaaipress.org/Papers/AAAI/2002/AAAI02-110.pdf). There is also no good approach to prune branches yet, because we want to optimize for total waste instead of bins used.

Advice on anything is very welcome!
