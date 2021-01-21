
## --------------------------------------------------------------------------
# This is a comment.
# We know because it is preceded
# by a hashtag, or 'octothorpe'.

# R ignores comments so you have
# a way to write down what you have
# done or what you are doing.

# This is useful for sharing
# code or just figuring out
# what you did.


## --------------------------------------------------------------------------
# Add 1 and 1 together
1 + 1


## --------------------------------------------------------------------------
# Example (run the following lines):
a <- 1
A <- 2

# Are these two things equal?
a == A


## ---------------------------------------------------------------
## # This won't work
## 1a <- 1
## 
## # But this one works
## # Try it by typing
## # a1,
## # print(a) or
## # show(a)
## # in the console:
## a1 <- 1
## 


## --------------------------------------------------------------------------
a <- 1
a <- 2

a



## --------------------------------------------------------------------------
T == TRUE


## --------------------------------------------------------------------------
a <- 1
a


## --------------------------------------------------------------------------
a <- c(1, 2, 3, 4, 5) # Make a vector of integers 1-5
print(a) # One way to look at our vector
show(a) # Another way to look at it
a # A third way to look at it
str(a) # Look at the structure, integer class


## --------------------------------------------------------------------------
# Define the same vector using a sequence
a <- seq(from = 1, to = 5, by = 1)
str(a)


## --------------------------------------------------------------------------
b <- c("a", "b", "c", "d", "e") # Make a character vector
b # Print it to the console
str(b) # Now it's a character vector
b <- as.factor(b) # But we can change if we want
b
str(b) # Look at the data structure


## ---- eval=FALSE-----------------------------------------------------------
## as.numeric(b)
## 
## # What did that do?
## ?as.numeric


## ---- message=FALSE, warning=FALSE-----------------------------------------
# The '==' compares the numeric vector to the factor one
c <- a == b
c
str(c)


## --------------------------------------------------------------------------
is.na(a) # We can check for missing values
is.finite(a) # We can make sure that all values are finite
!is.na(a) # The exclamation point means 'not'
a == 3 # We can see if specific elements meet a criterion
unique(b) # We can just look at unique values


## --------------------------------------------------------------------------
# This one just prints it
a[3]

# This one stores it in a new object
f <- a[3]


## --------------------------------------------------------------------------
b[b == "c"]
which(b == "c")


## --------------------------------------------------------------------------
a * .5 # Multiplication
a + 100 # Addition
a - 3 # Subtraction
a / 2 # Division
a^2 # Exponentiation
exp(a) # This is the same as 'e to the...'
log(a) # Natural logarithm
log10(a) # Log base 10


## --------------------------------------------------------------------------
b <- as.character(b)
paste(b, "AAAA", sep = "") # We can append text
paste("AAAA", b, sep = "") # We can do it the other way
paste("AAAA", b, sep = "--") # Add symbols to separate
gsub(pattern = "c", replacement = "AAAA", b) # We can replace text

e <- paste("AAAA", b, sep = "") # Make a new object
e # Print to console
substr(e, start = 5, stop = 5) # We can strip text (or dates, or times, etc.)


## --------------------------------------------------------------------------
length(a) # A has a length of 5, try it and check it
a # Yup, looks about right


## --------------------------------------------------------------------------
cbind(a, e)


## --------------------------------------------------------------------------
matrix(0, nrow = 3, ncol = 4)


## --------------------------------------------------------------------------
mat <- matrix(seq(1, 12), ncol = 3, nrow = 4)


## --------------------------------------------------------------------------
ncol(mat) # Number of columns
nrow(mat) # Number of rows
length(mat) # Total number of entries
mat[2, 3] # Value of row 2, column 3
str(mat)


## --------------------------------------------------------------------------
colnames(mat) <- c("first", "second", "third")
rownames(mat) <- c("This", "is", "a", "matrix")
mat
str(mat) # Take a look to understand


## -----------------------------------------------------------
otsego <- read.csv("data/physical.csv")


## ----eval = FALSE----------------------------------------------------------
## ls()


## ---- eval = FALSE---------------------------------------------------------
## ls() # We can use ls() to see what is in our environment
## head(otsego) # Look at the first six rows of data in the object
## nrow(otsego) # How many rows does it have?
## ncol(otsego) # How many columns?
## names(otsego) # What are the column names?
## str(otsego) # Have a look at the data structure
## summary(otsego) # Summarize the variables in the dataframe


## --------------------------------------------------------------------------
otsego[12, 4]


## --------------------------------------------------------------------------
otsego[12, "depth"]


## --------------------------------------------------------------------------
mean(otsego$temp[otsego$depth == 10.0], na.rm = TRUE)


## ---- warning=FALSE, message=FALSE-----------------------------------------
library(tidyverse)



## --------------------------------------------------------------------------
otsego <- group_by(otsego, month, depth)


## --------------------------------------------------------------------------
otsego_summary <- summarize(otsego, mean_temp = mean(temp), sd_temp = sd(temp))


## --------------------------------------------------------------------------
print(otsego_summary)


## ---- message = FALSE, warning = FALSE-------------------------------------
ggplot(otsego_summary, aes(x = mean_temp, y = depth)) +
  geom_point() +
  facet_wrap(~month)


## --------------------------------------------------------------------------
# Define a function to convert temperature
# in celcius to temperature in farenheit:
cToF <- function(cels) {
  faren <- cels * (9 / 5) + 32
  return(faren)
}


## --------------------------------------------------------------------------
# Test the function out.
# Here, we create a new variable in the
# otsego dataframe to hold the result
otsego$tempF <- cToF(otsego$temp)


## -------------------------------------------------------------------
## write.csv(x = otsego, file = "physicalF.csv")


## -------------------------------------------------------------------
## # Write the data set to a csv file.
## # We specify the object, the filename, and
## # we tell R we don't want rownames or quotes
## # around character strings. Finally, we tell
## # R to separate columns using a comma.
## 
## # We could do exactly the same thing, but use a ".txt"
## # file extensions, and we would get a comma-separated
## # text file if we needed one.
## write.table(
##   x = otsego, file = "physicalF.csv", row.names = FALSE,
##   quote = FALSE, sep = ","
## )


## ---- eval=FALSE-----------------------------------------------------------
## save(otsego, file = "otsego.rda")

