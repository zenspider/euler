require "maffs"

# Euler #22:

names = [""] + File.read("022/names.txt").scan(/\w+/).sort
p names.each_with_index.map { |name, index| name.euler22 * index }.sum
