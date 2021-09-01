dat
# how many observations are there?
dim(dat)
# ok, draw 100 numbers between 1 and 17640 randomly
idx <- sample(1:17640, size = 100, replace = FALSE)
# take these observations as subsample
dat_sample <- dat[idx, ]
# look at the BMI variable, if it is a numeric variable (it is)
str(dat_sample$bmiB)
# now compute the mean BMI of boys and girls
female <- dat_sample$bmiB[dat_sample$sex == "Weiblich"]
male <- dat_sample$bmiB[dat_sample$sex == "Männlich"]
t.test(female,male)
t.test(dat_sample$bmiB )
                                                                                                                                                                                                                                 
###THINGS FOR THE HOMEWORK

dat_sample$sys1
str(dat_sample$sys1)
is.factor(dat_sample$sys1)
summary(dat_sample$sys1)

dat_sample$sys2
str(dat_sample$sys2)
is.factor(dat_sample$sys2)
summary(dat_sample$sys2)


dat_sys1 <- as.numeric(dat_sample$sys1)
dat_sys2 <- as.numeric(dat_sample$sys2)


t.test(dat_sys1, dat_sys2 , paired = TRUE, alternative = "two.sided")

