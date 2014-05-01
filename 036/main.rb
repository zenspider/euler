require "maffs"

# Euler #036:

p (1..1_000_000).find_all { |n| n.palindrome? && n.to_s(2).palindrome? }.sum
