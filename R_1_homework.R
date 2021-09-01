####################################################################################
### Homework 1 
###
### Task: Download this R file, save it on your computer, and perform all the below tasks by inserting your answer in comments or R code below. After you are done, submit this file with your solutions on Moodle.
####################################################################################

## Exercise 1
## Save the mtcars data frame as an R data frame on your desktop. After saving it, load it again in RStudio.

## Exercise 2
## Find out some information about the mtcars data frame, for example what the different variable names mean.

## Exercise 3
## Use the help functions to find out, what the option 'main' means in the 'hist' function. Explain what it can be used for in one sentence, and try to use it in an example.

## Exercise 4
## Install any R package of your choice and try to find out what the functions in the R package can be used for.

## Exercise 5: import Excel file.
## Create your own dataset of a fictional study where you record age, sex, and height of 5 friends in an Excel file. Create an Excel file with this information, save the file on your laptop, import the file into RStudio, and save the dataframe as an R object (.RData file).

## Exercise 6 (optional)
## Use the read.csv() and the read_csv() functions to import the Pima diabetes csv dataset, and compare the datasets that are created in R. Are there differences, are there big differences, what are the differences?

dat <- read.csv(file = url("https://www.dropbox.com/s/4s4rhf6abda6gw2/Pima_diabetes.csv?dl=1"))
dat2 <- readr::read_csv(file = url("https://www.dropbox.com/s/4s4rhf6abda6gw2/Pima_diabetes.csv?dl=1"))
head(dat)
head(dat2)

## Exercise 7: Import and check data (optional)
## Download the NoShow csv data (KaggleV2-May-2016(1).csv) and import it into R. Now check the data, (i) whether all variables have been imported correctly and (ii) if all values in the dataset make sense and seem correct.
install.packages("ggplot2")
