#quick summary of everything
View(kiggs)
str(kiggs)
summary(kiggs)
head(kiggs)
table(kiggs$residence1)
table(kiggs$sex)
table(kiggs$age)
table(kiggs$sbp)
table(kiggs$bmi_metric)
table(kiggs$bmi_cat)
table(kiggs$shortsight)

# missing values
table(is.na(kiggs$sbp))


#2. number of observations with unique id
table(table(kiggs$ID))
761+19
799-780
#dat$BMI <- dat$BMI[!is.na(dat$BMI)]

#3. missing values of the following questions

table(is.na(kiggs$id))
table(is.na(kiggs$age))
table(is.na(kiggs$sbp))
table(is.na(kiggs$bmi_cat))
table(is.na(kiggs$shortsight))

#4. mean sbp

mean(kiggs$sbp)

new_var_sbp <- kiggs$sbp[!is.na(kiggs$sbp)]
table(new_var_sbp)
is.na(new_var_sbp)
table(is.na(new_var_sbp))
kiggs$sbp[new_var_sbp]
table(is.na(kiggs$sbp[new_var_sbp]))

is.numeric(kiggs$sbp[new_var_sbp])
as.numeric(as.character(kiggs$sbp[new_var_sbp]))
as.double(kiggs$sbp[new_var_sbp])

mean(as.numeric(as.character(kiggs$sbp[!is.na(kiggs$sbp)])))


round(mean(as.numeric(kiggs$sbp[new_var_sbp])), digits = 3)

kiggs$newsbp <- kiggs$sbp[!is.na(kiggs$sbp)]

#5. bmi

table(kiggs$bmi_metric)
mean(kiggs$bmi_metric)
str(kiggs$bmi_metric)

mean((kiggs$bmi_metric[!is.na(kiggs$bmi_metric)]))

#6. higher bmi

table(kiggs$bmi_metric > 40)

#7. t.test

bmi_female <- kiggs$bmi_metric[kiggs$sex == "girls"]
bmi_male <- kiggs$bmi_metric[kiggs$sex == "boys"]
t.test(bmi_male, bmi_female)

  mean(dat_sample$bmiB[dat_sample$sex == "Weiblich"], na.rm = TRUE)
mean(dat_sample$bmiB[dat_sample$sex == "Männlich"], na.rm = TRUE)

#8.
hist(((kiggs$bmi_metric[!is.na(kiggs$bmi_metric)])))

#10

str(kiggs$shortsight)
table(kiggs$shortsight)
87/585
table(kiggs$shortsight, kiggs$sex)
43/(43+297)
44/(44+288)
abc <- 43/(43+297)
abc1 <- 44/(44+288)
odds.boys <- abc/(1-abc)
odds.girls <- abc1/(1-abc1)
odds.boys
odds.girls
+