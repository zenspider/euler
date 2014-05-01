require "maffs"

# Euler #4:
# largest palindrome made frome the product of 2 3 digit numbers

r = (100..999)

p r.to_a.product(r.to_a).reverse.first(10000).find { |x,y| n = (x * y).to_s; n.reverse == n }.mult

catch :done do
  999.downto(100).each do |x|
    999.downto(100).each do |y|
      n = (x * y).to_s
      if n.reverse == n then
        p n
        throw :done
      end
    end
  end
end

r = (100..999).to_a.reverse
p r.find { |x| r.find { |y| n = (x * y).to_s; n.reverse == n } }

(100..999).each do |x|
  (100..999).each do |y|
    n = (x*y).to_s
    next unless n.reverse == n
    p n
    exit
  end
end
