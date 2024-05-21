rm(list = ls())

## Problem 1 ---- 
# Type the code below within R to generate a vector called vec1
# vec1 <- c(0, 2, 3, 0, 2, 11, 0, 7, NA)
# a) Remove the NA value, either by indexing or using the relevant function.
# b) Make a logical vector (e.g., either TRUE and FALSE) indicating the elements
# equal to zero as TRUE, and the remaining elements as FALSE.
# c) Use the logical vector you have created to pick out the non-zero values and store
# them in a vector called no_zero.
# d) Check how many non-zero values you have in vec1 by taking the length of the
# vector no_zero (using function length())

#Solution 1 ----

#Created the vector
vec1 <- c(0,2,3,0,2,11,0,7,NA)

#a) removed NA value
vec1 <- vec1[!is.na(vec1)]

#b) made a logical vector
logical_vec1 <- vec1 %in% 0

#c)Using logical vector no_zero vector is created
no_zero <- vec1[!logical_vec1]

#d)Fetching the length of no_zero vector
length(no_zero)

#Solution 1 END

## Problem 2 ---- 
# In this exercise you will take data from a table and store the information in a data frame,
# that you then will export to a plain-text (.csv) file which is easy to work with in R,
# and any other statistical package.
# a) Construct a data frame (manually in R) from the table given below, including the
# three variables which you name in R (remember that a vector in R is a column):
# - W (average wage/h, given by the numbers in the table)
# - YEAR (including the years for each observation, given in the table)
# - GENDER (including characters indicating "Female" or "Male" (hint: rep() is useful).
#           The number of rows should be as many as the number of values
#           in the table, e.g., the number of rows in your data frame should be 18!
#             Please note that the variable GENDER is based on the table below, but 
#           the character values "Female" and "Male" need be given in R.
# 
#     2003 2004 2005 2006 2007 2008 2009 2010 2011
# Average Wage/h
# Men 120 122 124 130 136 140 143 150 155
# Women 109 112 115 121 128 132 135 140 148

#Solution 2 ----

#a) Constructing the data frame

average_wage_per_hour <- c (120, 122, 124, 130, 136, 140, 143, 150, 155,
                            109, 112, 115, 121, 128, 132, 135, 140, 148)
year <- rep(c(2003:2011),2)
gender <- c(rep("Male",9),rep("Female",9))

df_csv <- data.frame( W = average_wage_per_hour,
                      YEAR = year,
                      GENDER = gender)

View(df_csv)  

#b) exporting the data frame as a comma separated (.csv) file

write.csv(df_csv,"employees_average_wage.csv", row.names = FALSE)

#Solution 2 END

## Problem 3 ----

# For this exercise you need to download the data file "Freedman.csv" from Learn.
# a) Import the data to R as data.frame() named Freedman.
# b) Use summary() and str() on the imported data.
# c) Sometimes variables that are numeric is read as character or integer,
# but it is possible to use as.numeric() on these variables to define them as
# numeric. All variables except for the variable City should be numeric, make
# sure they are. (Hint: Freedman$variable, and assign new variable)
# d) Using this data set, give the mean values of each column of data (not City).
# Remember that NA values must be accounted for in function mean()
# e) Retrieve the rows which have a non-white population larger than 30%.
# (This is what the column nonwhite gives us as information, % non-white)

#Solution 3 ----

#Freedman.csv is downloaded, and using setwd() downloaded path is set as current dir.
setwd("C:/Users/abdul/Documents/datasets/r_lab_assignment")
file_name <- "Freedman.csv"

#a) Importing the data to R as data.frame() named Freedman
Freedman <- data.frame(read.csv(file_name))

#b) Using summary() and str() on the imported data.
summary(Freedman)
str(Freedman)

#c) Type casted int fields to numeric
Freedman$population <- as.numeric(Freedman$population)
Freedman$density <- as.numeric(Freedman$density)
Freedman$crime <- as.numeric(Freedman$crime)

str(Freedman)

#d)Calculating mean value for all numeric fields.

###

#Creating a function to return mean by replacing NA with 0's if any
mean_value <- function(input_field) {
  return(mean(replace(input_field,is.na(input_field),0)))
}

#Calculating Mean values for all the fields using above function
mean_population <- mean_value(Freedman$population)
mean_nonwhite <- mean_value(Freedman$nonwhite)
mean_density <- mean_value(Freedman$density)
mean_crime <- mean_value(Freedman$crime)

print(paste('Mean value of Population is :',mean_population))
print(paste('Mean value of NonWhite is :',mean_nonwhite))
print(paste('Mean value of Density is :',mean_density))
print(paste('Mean value of Crime is :',mean_crime))

#e)Retrieving the rows with nonwhite population greater than 30%

Freedman[Freedman$nonwhite > 30,]

#Solution_3_END

## Problem 4 ----

# 4. For this exercise we will use the Prestige data frame in the package
# car.
# a) Install the package, if you haven't done it already, and load the data frame.
# Read the help file for the data to learn about the variables.
# (?Prestige)
# b) Select a subset of the data for occupations with more than 50% women
# and store this data.frame as an object sub_prestige_women.
# c) Use this subset and compute the average prestige score.
# d) Now compute the average prestige score for occupations with less than
# 50% women.
# e) For this final question below, use the complete Prestige data again
# (e.g., do not use sub_prestige_women).
# Make a for-loop to compute the average (mean) prestige score for
# the three different types of occupations. Automatically store the
# three means in a vector.
# (Hint: if you want a vector of the professions, there is a function unique() )

#Solution 4 ----

#a)Installing the car package, loading Prestige data to a data frame and view help of Prestige

install.packages("car")
library(car)
df_prestige <- data.frame(Prestige)

head(Prestige)

#b) Creating data frame sub_prestige_women
sub_prestige_women <- df_prestige[df_prestige$women > 50,]

#c) Averaging Prestige Score for sub_prestige_women
print(mean(sub_prestige_women$prestige))

#d) Averaging Prestige Score for for occupations with less than 50% women
print(mean(df_prestige$prestige[df_prestige$women < 50]))

#e) Mean of Prestige Score for all 3 occupations

df_split <- split.data.frame(df_prestige,f=df_prestige$type)

mean_vector <- c()

for (df_type in df_split) {
  mean_vector <- c(mean_vector, mean(df_type$prestige))
}

mean_vector

#Solution 4 END