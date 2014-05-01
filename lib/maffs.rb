require 'enumerator'
require 'open-uri'
require 'pp'

def time
  t0 = Time.now
  r = yield
  p Time.now - t0
  r
end

module Fib
  @fib = {}

  def self.fib(n)
    @fib[n] ||= case n
                when 1, 2 then
                    1
                else
                  fib(n-1) + fib(n-2)
                end
  end
end

class String
  def palindrome?
    self.reverse == self
  end
end

class Range
  alias to_ary to_a
end

class Integer
  def square
    self ** 2
  end

  def palindrome?
    self.to_s.palindrome?
  end

  def factor # FIX: I'm sure this is as inefficient as can be
    f = self.prime_factor
    [1] + (1..f.size).map { |n| f.combination(n).map(&:mult) }.flatten.uniq.sort
  end

  def factorial!
    return 1 if self < 2
    (2..self).mult
  end

  def prime_factor
    n, factors = self, []
    Integer.primes.each do |m|
      while n % m == 0
        factors << m
        n /= m
      end
      break if m == n
    end
    factors
  end

  def primes
    n = self
    sieve = (0..n).to_a
    sieve[0..1] = [nil,nil]

    2.upto(Math.sqrt(n).to_i) do |i|
      if sieve[i] then
        (2*i).step(n,i) do |j|
          sieve[j] = nil
        end
      end
    end

    sieve.compact
  end

  def self.primes
    @primes ||= 1_000.primes
  end

  def self.primes_up_to n
    @primes = n.primes
    @primes
  end

  def prime?
    self.class.primes.include? self
  end
end

module Enumerable
  def sum
    inject(&:+)
  end

  def mult
    inject(&:*)
  end

  def map_with_index
    Enumerable::Enumerator.new(self).with_index.map { |o, index|
      yield o, index
    }
  end

  def map_to_ints
    map(&:to_i)
  end
end

class String
  def euler22
    self.split(//).map { |s| s[0].ord - ?@.ord }.sum || 0
  end
end

class Array
  # intersection, non-uniqued
  def ^ other
    r = []
    (self & other).each do |obj|
      r.push(*([obj] * [self.count(obj), other.count(obj)].min))
    end
    r
  end

  # -, non-uniqed
  def % other
    result = self.dup
    (self & other).each do |obj|
      other.count(obj).times do
        result.delete_at(result.index(obj)) # my god this is stupid
      end
    end
    result
  end
end
