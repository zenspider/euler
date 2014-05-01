require "maffs"

p (1..1000).find_all { |n| n % 3 == 0 || n % 5 == 0 }.sum
