require "maffs"

# Euler #12:

class TriangleNumber
  include Enumerable
  def each
    n = sum = 0
    loop do
      n += 1
      sum += n
      yield sum
    end
  end
end

p TriangleNumber.new.find { |n| n % 10 == 0 and n.factor.size > 500 }
