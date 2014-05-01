# Euler #42:

# # 350 = 14 z's
# n = (1..350).map { |n| (n*n + n)/ 2 }
# a = eval "[#{File.read('words.txt')}]"
#
# w = a.map { |word| word.split(//).map { |s| s[0] - ?A + 1 }.sum }
# p w.count { |word| n.include? word }

