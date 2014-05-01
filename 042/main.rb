require "maffs"

# Euler #42:

# 350 = 14 z's
n = (1..350).map { |n| (n*n + n)/ 2 }
a = eval "[#{File.read('042/words.txt')}]"

w = a.map { |word| word.split(//).map { |s| s[0].ord - ?A.ord + 1 }.sum }
p w.count { |word| n.include? word }
