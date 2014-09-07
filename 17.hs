-- Problem 17
--
-- If the numbers 1 to 5 are written out in words: one, two, three, four,
-- five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written
-- out in words, how many letters would be used? 



euler17 = length1To1000

length1To1000 =
    (length "one") + lengthThousand + length1To999

length1To999 =
    (length1To9 * 100) + (lengthHundred * 9) + (lengthHundredAnd * 9 * 99) + (10 * length1To99)

length1To99 =
    length1To9 + length10To19 + (8 * length1To9) + (length10s * 10)

length1To9 =
    length "one" + length "two" + length "three" + length "four"
    + length "five" + length "six" + length "seven" + length "eight"
    + length "nine" 

length10To19 = 
    length "ten" + length "eleven" + length "twelve" + length "thirteen"
    + length "fourteen" + length "fifteen" + length "sixteen" 
    + length "seventeen" + length "eighteen" 
    + length "nineteen" 

length10s = 
    length "twenty" + length "thirty" + length "forty" +
    length "fifty" + length "sixty" + length "seventy" + 
    length "eighty" + length "ninety"


lengthHundred = length "hundred"

lengthHundredAnd = lengthHundred + (length "and")

lengthThousand = length "thousand"
