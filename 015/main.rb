require "maffs"

# Euler #15:
# How many routes are there through a 20x20 grid?
# (how many leaf nodes through a balanced binary tree 20 nodes deep?)
# NOT A TREE! because they come back together... this is a graph problem

#   6
#  3 3
# 1 2 1
#  1 1
#   1

def pascal n, r
  n.factorial! / (r.factorial! * (n - r).factorial!) # / stupid emacs
end

def euler15 n
  pascal((2*n), n)
end

p (1..8).map { |n| euler15 n } # => [2, 6, 20, 70, 252]
euler15 20                   # => 137846528820
