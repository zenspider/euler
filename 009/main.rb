# Euler #9:
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

