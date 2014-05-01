require "maffs"

# Euler #3:
# The prime factors of 13195 are 5, 7, 13 and 29.
# What is the largest prime factor of the number 600851475143 ?

p Integer.primes.reverse.find { |n| 600851475143 % n == 0 }
p 600851475143.prime_factor.max
