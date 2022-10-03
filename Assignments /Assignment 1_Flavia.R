################################################################################
# Title: Assignment 1

# Name of file: assignment_1.R
# A script containing instructions for Assignment 1, FOCD 2021 UMD/UM
# Author: Flavia Batista da Silva
# Date: Tue Sep 14, 2021
################################################################################
# Please write down the R command(s) needed after each question!
# Some questions require an interpretation of the results in addition to code!

# There is a total of 22 questions.


### Data types
# 1. Which command do you use to determine the type of an object?

#typeof()

# 2. What is the type of vector A?
A <- c("2", "3", "4", "5", "6", "7", "8")

typeof(A)

#Vector A is a character vector.

# 3. Convert A into an integer vector

A_converted <- as.integer(A)
typeof(A_converted)

# 4. Create an integer vector B containing the numbers one through ten

B <- (1:10)
B
typeof(B)

# 5. Create a new vector C from B which has the type "double"

C <- as.double(B)
C
typeof(C)

# 6. Change the third value of B to "3.5"

B1 <- replace(B, 3, 3.5)
B1
typeof(B1)

# 7. Did this affect the type of B? How?

#It did. It changed the type of the vector from "integer" to "double".


### Reading in data
# Download both the Angell.dta (Stata data format) dataset and the Angell.txt dataset from this website
# https://stats.idre.ucla.edu/stata/examples/ara/applied-regression-analysis-by-fox-data-files/

# 8. Read in the .dta version and store in an object called angell_stata

angell_stata <- read_dta ("angell.dta")

# 9. Read in the .txt version and store it in an object called angell_txt

angell_txt <- read.table("angell.txt")
View(angell_txt) #or head()

# 10. Drop the first five observations in the angell_txt object

angell_txt[-c(1:5), ]


# 11. Select columns 2 and 3 of the agell_stata object and store them in a new object called angell_small

angell_small <- angell_stata [, 2:3]
head(angell_small)


# R comes also with many built-in datasets. The "MASS" package, for example, comes with the "Boston" dataset
# 12. Install the "MASS" package, load the package. Then, load the Boston dataset

install.packages ("MASS")
library(MASS)
head (Boston)

# 13. What is the type of the Boston object?

typeof(Boston)

#Boston object is a list.

# 14. What is the class of the Boston object?

class(Boston)

#Boston object is a dataframe.

### Basic data summarizing and description

# 15. How many of the suburbs in the Boston data set bound the Charles river?

View(Boston)
nrow(subset(Boston, chas ==1))

#35 suburbs in the Boston data set bound the Charles River.

# 16. Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates?
# Pupil-teacher ratios? Comment on the range of each variable.

#Crime rates

summary(Boston$crim)


#There might be suburbs of Boston with significant high crime rates as the variables range from 0 (minimum value) to 80 (maximum value).
#The majority of the suburbs seem to have a per capita crime between 0 and 3, with significant high outliers.

#Tax rates

summary((Boston$tax))

# In this case, there might be suburbs of Boston with significant low tax rates and just a few with high tax rates as the variables ranges from 187 (minimum value) to 711 (maximum value) and the mean is 408.2.

#Pupil-teacher ratios

summary(Boston$ptratio)

#In this case, the range of the variable seems really normal, so pupil-teacher ratios in Boston are relatively distributed among the suburbs. There seem not to be outliers in this case.

# 17. What is the median pupil-teacher ratio among the towns in this data set that
# have a per capita crime rate larger than 1 ?


median(Boston$ptratio[which(Boston$crim > 1)])


#The median pupil-teacher ratio is 20.2.


### Functions
# 18. Write a function that calculates the square root of an integer

square_root <- function(a) {
  x <- a/2
  while (TRUE) {
    y <- (x + a / x) / 2
    if (y == x) break
    x <- y
  }
  return(y)
}

square_root(9)

# 19. Write a function that calculates 95% confidence intervals for a point estimate.
# The function should be called "my_CI"
# When called with "my_CI(2, 0.2)", the output of the function should read
# "The 95% CI upper bound of point estimate 2 with standard error 0.2 is 2.392. The lower bound is 1.608."
# Note: the function should take a point estimate and its standard error as arguments
# You may use the formula for 95% CI: point estimate +/- 1.96*standard error)
# Pasting text in R: paste() and paste0()

my_CI <- function(x, y) {
  lower <- x - 1.96 * y
  upper <- x + 1.96 * y
  paste("The 95% CI upper bound of point estimate", x, "with standard error", y,
        "is", upper,".", "The lower bound is", lower,".")
}

my_CI (2, 0.2)


# 20. Write a function that converts all negative numbers in the following dataset into NA
# Use as little code as possible and try to avoid code repetition
set.seed(1002)
df <- data.frame(replicate(10, sample(c(1:10, c(-99,-98,-5)), 6, rep = TRUE)))
names(df) <- letters[1:6]
df

class(df)

fun <- function (x) {
  x <- replace(x, x < 0, NA)
  print(x)
}

fun(df)

# 21. Use your function to convert all negative numbers in the dataset into NA without changing the class of the object

#The class did not change with my function.

class (df)

# 22. Change the function you wrote above such that it turns any negative number into NA!

#I already did this for q20. 

