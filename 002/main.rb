require "maffs"

p (1..33).map { |n| Fib.fib(n) }.reject { |n| n % 2 == 1 }.sum
