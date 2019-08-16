# *************************************************************************************************************
# Text Mining - Dealing with Regular Expressions
#
# The code deomonstrates the following regular expressions in R
# grep, grepl, regexpr, gregexpr, sub and gsub
# 
# **************************************************************************************************************
rm(list = ls()) # clean the environment

# grep,grepl, regexpr and gregexpr search for matches to argument pattern  within each element of a character vector
# sub and gsub perform replacement of the first and all matches respectively

# grep(pattern, x)
# ---------------------

# grep searches and returns the position of the match from a vector

# Searches for either a or u from the character class - which is a list of characters enclosed between [ and ] which matches a character in the list
grep("[au]", c("Harry Potter", "Game of thrones", "Lord of the Rings")) 
# Returns 1 and 2 because "a" is present in first element and second element but neither a nor u is present in the third element

# ^ symbol : it matches any character not in the list
# Lists the characters that does not contain "Harry Potter"
grep("[^Harry Potter]", c("Harry Potter", "Game of thrones", "Lord of the Rings"))

# [letters] refer to subset of strings for all alphabets - essentially checks whether character data
grep("[letters]",  c("Harry Potter", "45", "Lord of the Rings")) # returns 1 and 3 because 2 has a number and not just characters
grep("[:lower:]",  c("Harry Potter", "Game of thrones", "LORD OF THE RINGS"))
# the above returns only 1 and 2 because they have lower case alphabets

# to check whether punctuation is present in any set of characters
#  ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ `{ | } ~.
grep("[[:punct:]]",  c("Harry/;;Potter$;>=<", "a1234", "Lordofthe"))

# A period (.) represents any single character
grep("t.e", c("Harry Potter", "Game of thrones", "Lord of the Rings"))
# The above gives you all items which has a single character between t and e
grep("L..d", c("Harry Potter", "Game of thrones", "Lord of the Rings")) # gives you the item which has two character between L and d

name <- c("a.txt", "pqr", "p.txt")
grep(".txt", name)

# '.' acts as a metacharacter, means any character
# If we specifically want only the string with the "." then we need to escape the metacharacter nature of the "."
grep(".", c("abc","de", "f.g")) # This shows me all values, because in general "." means any character
# so if we want to get only the number which has "." in it, we do
grep("\\.", c("abc","de", "f.g"))

# grepl(pattern, x)
# ---------------------

# grepl instead of position, it returns the logical value (TRUE/FALSE) instead of the position
grepl("[au]", c("Harry Potter", "Game of thrones", "Lord of the Rings")) 
# This will return TRUE, TRUE and FALSE based on the presence of a or u
grepl("[bu]", c("Harry Potter", "Game of thrones", "Lord of the Rings"))

# regexpr(pattern, x)
# ---------------------

# It finds the character position of the first instance of the pattern within text

regexpr("#", c("Harry #Potter", "Game of #thrones", "Lord of the #rings")) 
# The above returns position 7 ( for the first element), position 9 and position 13 respectively
regexpr("(Harry+)", c("Harry #Potter", "Game of #thrones", "Lord of the #rings")) 
# The above evaluates the first occurence of "Harry" and any word after that 

# Position of first instance "." in the strings
regexpr("\\.", c("abc","de","f.g."))

# similarly for punctuation
regexpr("[[:punct:]]", c("Harry/;;Potter$;>=<", "a1234", "Lord of the rings"))

# gregexpr(pattern, x)
# ---------------------

# This is similar to regexpr but it finds all instances of pattern
gregexpr("#", c("Harry #Potter#", "Game of #thr#ones", "Lord of #the #rings")) 
# This states in the first string the '#' is in position 7 and 14 and so on for 2nd and 3rd string

# sub (pattern, replacement)
# ---------------------
# - It replaces a given string with another string
# - Only the first match in each string element is replaced.
# - If no matches could be found in some strings, those are copied into the result vector unchanged

# The sub function has three required parameters
# a string with regular expression
# a string with the replacement text
# and the input vector

sub("(Th+)", "e", c("The mountain The", "The hill hill", "The city without pollution is The peaceful city", "TheThe"), perl = TRUE)

# \1 - regular expression for the same character
sub("(Th+)", "\\1e", c("The mountain The", "The hill hill", "The city without pollution is The peaceful city", "TheThe"), perl = TRUE)


# gsub (pattern, replacement)
# ---------------------
# - It replaces a given string with another string
# - All the matches in each string element is replaced

gsub("(Th+)", "e", c("The mountain The", "The hill hill", "The city without pollution is The peaceful city", "TheThe"), perl = TRUE)
