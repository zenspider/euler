require "maffs"

# Euler #5:

results = []
(2..20).map { |n| n.prime_factor }.each do |f|
  results.push(*(f % results)) if results ^ f != f
end

p results.sort.mult
