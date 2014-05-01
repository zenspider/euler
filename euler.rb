#!/usr/bin/ruby -w
# -*- coding: utf-8 -*-

require "maffs"

# Euler #2:
# p (1..33).map { |n| Fib.fib(n) }.reject { |n| n % 2 == 1 }.sum

# Euler #3:
# The prime factors of 13195 are 5, 7, 13 and 29.
# What is the largest prime factor of the number 600851475143 ?

# p Integer.primes.reverse.find { |n| 600851475143 % n == 0 }
# p 600851475143.prime_factor.max

# euler #4
# largest palindrome made frome the product of 2 3 digit numbers

# r = (100..999)

# p r.to_a.product(r.to_a).reverse.first(10000).find { |x,y| n = (x * y).to_s; n.reverse == n }.mult

# catch :done do
#   999.downto(100).each do |x|
#     999.downto(100).each do |y|
#       n = (x * y).to_s
#       if n.reverse == n then
#         p n
#         throw :done
#       end
#     end
#   end
# end
#
# r = (100.999).to_a.reverse
# p r.find { |x| r.find { |y| n = (x * y).to_s; n.reverse == n } }

# (100..999).each do |x|
#   (100..999).each do |y|
#     n = (x*y).to_s
#     next unless n.reverse == n
#     p n
#     exit
#   end
# end

# euler #5
# results = []
# (2..20).map { |n| n.prime_factor }.each do |f|
#   results.push(*(f % results)) if results ^ f != f
# end
# p results.sort.mult

# euler #6:
# p((1..100).sum ** 2 - (1..100).map { |n| n ** 2 }.sum)

# euler #7:
# p 1_000_000.primes[10000]

# euler #8:
# find the biggest mult of 5 consecutive digits:

# p <<-X.scan(/\d/).map_to_ints.enum_cons(5).map(&:mult).max
# 73167176531330624919225119674426574742355349194934969835203127745063262395783
# 18016984801869478851843858615607891129494954595017379583319528532088055111254
# 06987471585238630507156932909632952274430435576689664895044524452316173185640
# 30987111217223831136222989342338030813533627661428280644448664523874930358907
# 29629049156044077239071381051585930796086670172427121883998797908792274921901
# 69972088809377665727333001053367881220235421809751254540594752243525849077116
# 70556013604839586446706324415722155397536978179778461740649551492908625693219
# 78468622482839722413756570560574902614079729686524145351004748216637048440319
# 98900088952434506585412275886668811642717147992444292823086346567481391912316
# 28245861786645835912456652947654568284891288314260769004224219022671055626321
# 11110937054421750694165896040807198403850962455444362981230987879927244284909
# 18884580156166097919133875499200524063689912560717606058861164671094050775410
# 0225698315520005593572972571636269561882670428252483600823257530420752963450
# X

# euler #9:
# # brute force:
# p (3..400).map { |a|
#   (a..400).map { |b| [a, b, 1000-a-b] if (a+b-1000)**2 == a*a + b*b }
# }.flatten.compact.mult
# # mathy:
# # given:
# #
# #      1000 = n
# #   aa + bb = c*c
# # a + b + c = n
# #
# # solve for a:
# #
# # n - a - b = c
# #   aa + bb = (n - a - b)^2
# #   aa + bb = nn - na - nb - na + aa + ab - nb + ab + bb
# #   aa + bb = nn - 2na - 2nb + aa + 2ab + bb
# #         0 = nn - 2na - 2nb + 2ab
# # 2na - 2ab = nn - 2nb
# # 2a(n - b) = nn - 2nb
# #         a = (nn - 2nb) / 2(n - b)
# #         a = n(n/2 - b) / (n - b)
#
# n = 1000
# a = (3..n/2).find { |b| n * (n/2 - b) % (n - b) == 0 }
# b = n * (n/2 - a) / (n - a)
# c = n-a-b
# p [a, b, c]

# euler #10:
# p 2_000_000.primes.sum

# euler #11: humans are better than computers at some things...
# FIX: numbers is now undefined
# n = numbers.split(/\n/).map { |s| s.split.map_to_ints }
# p n.map { |row| row.enum_cons(4).map { |quad| [quad.mult, quad] }.max }.max
# n = n.transpose
# p n.map { |row| row.enum_cons(4).map { |quad| [quad.mult, quad] }.max }.max
#
# or just fucking eyeball it: :D
# 70600674

# euler #12:
# class TriangleNumber
#   include Enumerable
#   def each
#     n = sum = 0
#     loop do
#       n += 1
#       sum += n
#       yield sum
#     end
#   end
# end
#
# p TriangleNumber.new.find { |n| n % 10 == 0 and n.factor.size > 500 }

# euler #13:
# url = "http://projecteuler.net/index.php?section=problems&id=13"
# p URI.parse(url).read.scan(/\d{50}/).map_to_ints.sum.to_s[0, 10]

# euler #14:

# n  n/2 (n is even)
# n  3n + 1 (n is odd)
#
# Using the rule above and starting with 13, we generate the following sequence:
#
# 13  40  20  10  5  16  8  4  2  1

class Collatz
  include Enumerable

  @@n = {}
  @@c = {}

  def initialize start
    @curr = start
  end

  def each
    loop do
      yield @curr
      break if @curr == 1
      @curr = collatz @curr
    end
  end

  def collatz n
    @@n[n] ||= n.odd? ? (3 * n + 1) : (n / 2)
  end

  def self.size n
    @@c[n] ||= begin
                 c = self.new
               end
  end
end

# p :euler => 14
# max = (2..1_000).max_by { |n|
#   s = Collatz.new(n).map.to_a.size
#   p n => s if n % 1000 == 0
#   s
# }
# p max => Collatz.new(max).map.to_a
#
# p 871 => Collatz.new(871).map.to_a == [871, 2614, 1307, 3922, 1961, 5884, 2942, 1471, 4414, 2207, 6622, 3311, 9934, 4967, 14902, 7451, 22354, 11177, 33532, 16766, 8383, 25150, 12575, 37726, 18863, 56590, 28295, 84886, 42443, 127330, 63665, 190996, 95498, 47749, 143248, 71624, 35812, 17906, 8953, 26860, 13430, 6715, 20146, 10073, 30220, 15110, 7555, 22666, 11333, 34000, 17000, 8500, 4250, 2125, 6376, 3188, 1594, 797, 2392, 1196, 598, 299, 898, 449, 1348, 674, 337, 1012, 506, 253, 760, 380, 190, 95, 286, 143, 430, 215, 646, 323, 970, 485, 1456, 728, 364, 182, 91, 274, 137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1]

############################################################
# n = 13
# until n == 1 do
#   p n
#   n = Collatz.collatz n
# end

# euler #15
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

# p (1..8).map { |n| euler15 n } # => [2, 6, 20, 70, 252]
# euler15 20                   # => 137846528820

# euler # 16:

# (2 ** 1000).to_s.split(//).map(&:to_i).inject(:+) # => 1366


# euler # 17:

# class Integer
  # WORDS = { 0 => "", 1 => "one", 2 => "two", 3 => "three", 4 =>
  #   "four", 5 => "five", 6 => "six", 7 => "seven", 8 => "eight", 9 =>
  #   "nine", 10 => "ten", 11 => "eleven", 12 => "twelve", 13 =>
  #   "thirteen", 14 => "fourteen", 15 => "fifteen", 16 => "sixteen", 17
  #   => "seventeen", 18 => "eighteen", 19 => "nineteen", 20 =>
  #   "twenty", 30 => "thirty", 40 => "forty", 50 => "fifty", 60 =>
  #   "sixty", 70 => "seventy", 80 => "eighty", 90 => "ninety", 100 =>
  #   "one hundred", 200 => "two hundred", 300 => "three hundred", 400
  #   => "four hundred", 500 => "five hundred", 600 => "six hundred",
  #   700 => "seven hundred", 800 => "eight hundred", 900 => "nine
  #   hundred", 1000 => "one thousand", }

#   def to_words
#     a = []
#     n = self
#     big = false
#     [1000, 100, 10, 1].each do |unit|
#       if WORDS[n] then
#         a << WORDS[n]
#         break
#       else
#         m = (n / unit) * unit
#         if WORDS[m] then
#           a << WORDS[m]
#           big = true if unit == 100 and m != 0
#         else
#           warn "unhandled: #{m}"
#         end
#       end
#       n %= unit
#     end
#
#     if big and a.length > 1 then
#       a[-1,0] = "and"
#     end
#
#     a
#   end
# end
#
# p 917.to_words
# p 342.to_words.join.delete(' ').length
# p 115.to_words.join.delete(' ').length
# x = (1..1000).map(&:to_words)
# p x.join.delete(' ').length

# euler # 18:

# triangle = <<-TRIANGLE
#                             75
#                           95  64
#                         17  47  82
#                       18  35  87  10
#                     20  04  82  47  65
#                   19  01  23  75  03  34
#                 88  02  77  73  07  63  67
#               99  65  04  28  06  16  70  92
#             41  41  26  56  83  40  80  70  33
#           41  48  72  33  47  32  37  16  94  29
#         53  71  44  65  25  43  91  52  97  51  14
#       70  11  33  28  77  73  17  78  39  68  17  57
#     91  71  52  38  17  14  91  43  58  50  27  29  48
#   63  66  04  68  89  53  67  30  73  16  69  87  40  31
# 04  62  98  27  23  09  70  98  73  93  38  53  60  04  23
# TRIANGLE
#
# node = Struct.new :n, :l, :r, :m do
#   def self.compute t
#     t.each_with_index { |row, i|
#       row.each_with_index { |node, j|
#         node.r = t[i+1][j+1] rescue nil
#         node.l = t[i+1][j+0] rescue nil
#       }
#     }
#
#     t.reverse.each do |row|
#       row.each do |node|
#         max = if node.l then
#                 if node.l.m >= node.r.m then
#                   node.l.m
#                 else
#                   node.r.m
#                 end
#               else
#                 0
#               end
#         node.m = node.n + max
#       end
#     end
#
#     t[0][0].m # => 1074
#   end
#
#   def inspect
#     "n(#{n}, #{m})"
#   end
# end

# triangle = triangle.split(/\n/).map { |l|
#   l.scan(/\d+/).map { |s| node.new s.to_i }
# }
#
# p node.compute triangle

# euler # 19:
# p (1901..2000).to_a.product(1..12).count { |y,m| Date.new(y,m,1).wday == 0 }

# euler # 20
# Find the sum of the digits in the number 100!

# p 100.factorial!.to_s.scan(/\d/).map_to_ints.sum

# euler # 21:
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

# Euler #22
# names = [""] + File.read("names.txt").scan(/\w+/).sort
# p names.map_with_index { |name, index| name.euler22 * index }.sum

# Euler #24:

# p (0..9).to_a.permutation.to_a[1_000_000 - 1].join

# Euler #42:

# # 350 = 14 z's
# n = (1..350).map { |n| (n*n + n)/ 2 }
# a = eval "[#{File.read('words.txt')}]"
#
# w = a.map { |word| word.split(//).map { |s| s[0] - ?A + 1 }.sum }
# p w.count { |word| n.include? word }

# Euler #67

# p (1..1_000_000).find_all { |n| n.palindrome? && n.to_s(2).palindrome? }.sum

# Euler #67:

# triangle = File.read("triangle.txt").split(/\n/).map { |l|
#   l.scan(/\d+/).map { |s| node.new s.to_i }
# }
#
# p node.compute triangle

# Euler #124:

# def rad n
#   n.prime_factor.uniq.mult || 1
# end
#
# p rad 21417
# p (1..10).map { |n| [ rad(n), n ] }.sort
# p (1..100_000).map { |n| [ rad(n), n ] }.sort[9_990..10010]

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

# p euler125(10**3) == 4164
#
# p euler125(10**4)
# p euler125(10**5)
# p euler125(10**6)
# p euler125(10**7)
# p euler125 10**8

# Euler #341: DONE: in mathematica

# The Golomb's self-describing sequence {G(n)} is the only
# nondecreasing sequence of natural numbers such that n appears
# exactly G(n) times in the sequence. The values of G(n) for the first
# few n are

# n     1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 …
# G(n)  1 2 2 3 3 4 4 4 5  5  5  6  6  6  6 …
# You are given that G(10**3) = 86, G(10**6) = 6137.
# You are also given that ΣG(n**3) = 153506976 for 1 <= n < 10**3.
# Find ΣG(n**3) for 1 <=  n < 10**6.

# g = [nil, 1, 2, 2]
#
# (3..1000).each do |n|
#   g.push([n] * g[n])
#   g = g.flatten
# end
# g.compact!
#
# g.first(999)

# TODO: invert logic and look at the values, they show how far you can
# push forward rather than look backward

# one = Rational 1
#
# require 'rational'
# ns = 1000.primes.map { |n|
#   one / n
# }
#
# n = ns.last * (10 ** 1000)
# p n.to_i

