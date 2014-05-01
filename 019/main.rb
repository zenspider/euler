require "maffs"

# Euler #19:

p (1901..2000).to_a.product(1..12).count { |y,m| Date.new(y,m,1).wday == 0 }
