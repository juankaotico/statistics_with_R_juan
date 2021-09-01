#change variable type
dat$lunchtime <- as.Date(dat$lunchtime)
#create a new variable
dat$BMI <- dat$weight/(dat$height**2)
#remove/add or replace
dat$BMI [1] <- 20
#create a new variable using bolean
#create a new BMI for extreme cases
#evaluate which people have more than 50
dat$BMIOutlier <- 0 #set the vector to 0
dat$BMIOutlier[dat$BMI > 50] <- 1 #set to 1 in those bigger than 50
# check datasets
dat[!dat$Age ==0,] # the age can not be == to 0
dat_female <- dat[dat$Gender == "F", ] #create the female classification
dat_final <- data.frame(ID=dat_female$PatientId, Age = dat_female$Age, NoShow = dat_female$No-show) #create the final frame
#Goal of the study
##To describe your study sample regarding its main characteristics
###Goals for student
    #Compute plots (and tables) to understand the characteristics, variables, distribution and associations.
    # presenti to others, describing and presenting the tables
load(file = url("https://www.dropbox.com/s/32pyh375kjkqv14/NoShowdata.RData?dl=1"))
View(NoShowdata)
#Absolut frequencies
table(NoShowdata$Gender)
table(NoShowdata$Scholarship)
#relative frequencies
table(NoShowdata$Gender)/length(NoShowdata$Gender)
table(NoShowdata$Scholarship)/length(NoShowdata$Scholarship)
table(NoShowdata$Gender, NoShowdata$Scholarship)
#descriptive stadistics of nominal variables
  ##frequencies
#descriptive stadistics of ordinal variables
  ##frequencies
  ##minimum, maximum, median
  ##range, quantiles, IQR, median absolute deviation (MAD)
#descriptive stadistics of ordinal variables
  ##mean, median, min, max
  ##range, quantiles, IQR, MAN, SD, variance
  ##(Skewness, kutrosis)
#how do you do report?
##scale of variables
  #Ordinal variables
    #Bar plot, mosaic plot (if not many categories)
    #Histogram (if there are many categories; might be not entirely "correct")
    #Boxplot
    #Scatterplots
  #Metric variables
    #Histogram, boxplot
    #Scatterplots
    #Quantile-quantile plots


library(qwraps2)
options(qwraps2_markup = "markdown")
our_summary <-list("Diabetes" =list("yes" = ~ qwraps2::n_perc0(.data$Diabetes == "yes"),"no" = ~ qwraps2::n_perc0(.data$Diabetes == "no")),"Age" =list("Median (IQR)" = ~ median_iqr(.data$Age)), "BMI" =list("Mean (SD)" = ~ qwraps2::mean_sd(.data$BMI)))
summary_table(Pima_diabetes, our_summary)
