require "maffs"

# Euler #6:

p((1..100).sum ** 2 - (1..100).map { |n| n ** 2 }.sum)
