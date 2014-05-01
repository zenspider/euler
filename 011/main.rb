# Euler #11:

# humans are better than computers at some things...
# FIX: numbers is now undefined

# n = numbers.split(/\n/).map { |s| s.split.map_to_ints }
# p n.map { |row| row.enum_cons(4).map { |quad| [quad.mult, quad] }.max }.max
# n = n.transpose
# p n.map { |row| row.enum_cons(4).map { |quad| [quad.mult, quad] }.max }.max
#
# or just fucking eyeball it: :D

p 70600674
