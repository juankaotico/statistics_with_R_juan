####################################################################################
### Exercise 1                                                                   
### Run all the code below and look at the results to understand what R is doing 
####################################################################################

# (1) Everything behind a '#' is a comment and will not be run

# (2) R can be used as a calculator:

1 + 1
1 / 1
1/2
1*1
2^2
2**2 # same as ^
log(10) # log with bases e
log10(10) # log with basis 10
exp(1) # e - exponential function
5 + 10/2 - 4 + exp(1)
sqrt(10) # square root
abs(-1) # absolute value

round(1.4534534) # decimals with '.' not ',' !
?round
round(1.4534534, digits = 2)
round(1.4534534, 2)

# (3) R does not care about spacing (but you might!)

1+1
1  + 1
1+             1

# (4) Code can be written over multiple lines, then R waits for you to finish the equation

1 + 
  1

(1
  +2)

# but the following doesn't work. Why?
1
+2

# (5) '<-' can be used to assign a value (on the right hand) to an object (on the left), e.g. assign x the value 1:

x <- 1
x

# (6) R is case-sensitive, i.e. X is so far not defined:

X
x

# (7) Some data frames are automatically available in R, e.g. the mtcars dataset. 
# (7a) It can be visualized with

mtcars

# (7b) Its first lines only can be shown using the function head():

head(mtcars)


hist(c(1,1,1,2,4,2,5,2,6,7,8,9,101,100))
