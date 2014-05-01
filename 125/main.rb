require "maffs"

# Euler #125:

def euler125 max
  iter = (max ** (1/2.07)).to_i # 2.07 was the safest I could use for limit
  squares = (1..iter).map(&:square)

  (2..iter).map { |m|
    warn m if m % 100 == 0
    squares.each_cons(m).map(&:sum).select { |n| n < max && n.palindrome? }
  }.flatten.uniq.sum
end

# Find the sum of all the numbers less than 10^8 that are both
# palindromic and can be written as the sum of consecutive squares.

p euler125(10**3) == 4164

p euler125(10**4)
p euler125(10**5)
p euler125(10**6)
p euler125(10**7)
p euler125 10**8
