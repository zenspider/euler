# -*- coding: utf-8 -*-

require "maffs"

# Euler #341:
# DONE: in mathematica

# The Golomb's self-describing sequence {G(n)} is the only
# nondecreasing sequence of natural numbers such that n appears
# exactly G(n) times in the sequence. The values of G(n) for the first
# few n are

# n     1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 …
# G(n)  1 2 2 3 3 4 4 4 5  5  5  6  6  6  6 …
# You are given that G(10**3) = 86, G(10**6) = 6137.
# You are also given that ΣG(n**3) = 153506976 for 1 <= n < 10**3.
# Find ΣG(n**3) for 1 <=  n < 10**6.

g = [nil, 1, 2, 2]

(3..1000).each do |n|
  g.push([n] * g[n])
  g = g.flatten
end
g.compact!

p g.first(999)

# TODO: invert logic and look at the values, they show how far you can
# push forward rather than look backward

one = Rational 1

require 'rational'
ns = 1000.primes.map { |n|
  one / n
}

n = ns.last * (10 ** 1000)
p n.to_i
