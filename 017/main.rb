# Euler #17:

class Integer

  WORDS = {
           0 => "", 1 => "one", 2 => "two", 3 => "three", 4 => "four",
           5 => "five", 6 => "six", 7 => "seven", 8 => "eight",
           9 => "nine", 10 => "ten", 11 => "eleven", 12 => "twelve",
           13 => "thirteen", 14 => "fourteen", 15 => "fifteen",
           16 => "sixteen", 17 => "seventeen", 18 => "eighteen",
           19 => "nineteen", 20 => "twenty", 30 => "thirty",
           40 => "forty", 50 => "fifty", 60 => "sixty", 70 => "seventy",
           80 => "eighty", 90 => "ninety", 100 => "one hundred",
           200 => "two hundred", 300 => "three hundred",
           400 => "four hundred", 500 => "five hundred", 600 => "six hundred",
           700 => "seven hundred", 800 => "eight hundred",
           900 => "nine hundred", 1000 => "one thousand",
          }

  def to_words
    a = []
    n = self
    big = false
    [1000, 100, 10, 1].each do |unit|
      if WORDS[n] then
        a << WORDS[n]
        break
      else
        m = (n / unit) * unit
        if WORDS[m] then
          a << WORDS[m]
          big = true if unit == 100 and m != 0
        else
          warn "unhandled: #{m}"
        end
      end
      n %= unit
    end

    if big and a.length > 1 then
      a[-1,0] = "and"
    end

    a
  end
end

p 917.to_words
p 342.to_words.join.delete(' ').length
p 115.to_words.join.delete(' ').length
x = (1..1000).map(&:to_words)
p x.join.delete(' ').length
