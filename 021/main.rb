# Euler #21:
# class Integer
#   def divisors
#     (1..self/2).find_all { |n| self % n == 0 ? [n, self/n] : nil }.flatten.compact
#   end
#
#   def div_sum
#     divisors.sum
#   end
# end
#
# p (2..10_000).find_all { |n| s1 = n.div_sum; n != s1 && s1.div_sum == n }.sum

