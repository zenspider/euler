require "maffs"

# Euler #124:

def rad n
  n.prime_factor.uniq.mult || 1
end

p rad 21417
p (1..10).map { |n| [ rad(n), n ] }.sort
p (1..100_000).map { |n| [ rad(n), n ] }.sort[9_990..10010]
