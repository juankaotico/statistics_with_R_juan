##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
### Aim: Check how the estimates of the regression coefficients, their standard error estimates, and the R^2 of the linear regression are affected by multicollinearity

library(MASS)

##################################################################################################################################################################
## Scenario 1: no correlation between the X variables

b1 <- b2 <- b3 <- NULL
b1_SE <- b2_SE <- b3_SE <- NULL
r2 <- NULL
for(i in 1:10000){
  print(i)
  Sigma <- matrix(c(1, 0, 0, 
                    0, 1, 0, 
                    0, 0, 1), 3, 3)
  help <- mvrnorm(n = 1000, mu = c(0, 0, 0), Sigma = Sigma, tol = 1e-6, empirical = FALSE)
  X1 <- help[,1]
  X2 <- help[,2]
  X3 <- help[,3]
  Y	<- 0.3*X1 + 0.3*X2 + 0.3*X3 + rnorm(1000,mean=0,sd=2)	
  help <- summary(lm(Y~X1+X2+X3))
  r2[i] <- help$r.squared
  b1[i]    <- help$coefficients[2,1]
  b2[i]    <- help$coefficients[3,1]
  b3[i]    <- help$coefficients[4,1]
  b1_SE[i] <- help$coefficients[2,2]
  b2_SE[i] <- help$coefficients[3,2]
  b3_SE[i] <- help$coefficients[4,2]
}

> summary(r2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.01862 0.05529 0.06490 0.06577 0.07557 0.12867 

> summary(b1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02428 0.25647 0.29940 0.29908 0.34163 0.55342 
> summary(b2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.06005 0.25825 0.29930 0.30011 0.34221 0.54238 
> summary(b3)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.0293  0.2589  0.2993  0.3000  0.3426  0.5471 
 
> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05642 0.06199 0.06332 0.06336 0.06469 0.07197 
> summary(b2_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05532 0.06201 0.06335 0.06338 0.06473 0.07205 
> summary(b3_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05644 0.06205 0.06337 0.06340 0.06473 0.07156 
 
> sd(b1)
[1] 0.06327937
> sd(b2)
[1] 0.06321049
> sd(b3)
[1] 0.06336711

##################################################################################################################################################################
## Scenario 2: correlation of 0.99 between X1 and X2

library(MASS)
b1 <- b2 <- b3 <- NULL
b1_SE <- b2_SE <- b3_SE <- NULL
r2 <- NULL
for(i in 1:10000){
  print(i)
  Sigma <- matrix(c(1, 0.99, 0, 
                    0.99, 1, 0, 
                    0, 0, 1), 3, 3)
  help <- mvrnorm(n = 1000, mu = c(0, 0, 0), Sigma = Sigma, tol = 1e-6, empirical = FALSE)
  X1 <- help[,1]
  X2 <- help[,2]
  X3 <- help[,3]
  Y	<- 0.3*X1 + 0.3*X2 + 0.3*X3 + rnorm(1000,mean=0,sd=2)	
  help <- summary(lm(Y~X1+X2+X3))
  r2[i] <- help$r.squared
  b1[i]    <- help$coefficients[2,1]
  b2[i]    <- help$coefficients[3,1]
  b3[i]    <- help$coefficients[4,1]
  b1_SE[i] <- help$coefficients[2,2]
  b2_SE[i] <- help$coefficients[3,2]
  b3_SE[i] <- help$coefficients[4,2]
}

> summary(r2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.04253 0.09067 0.10240 0.10314 0.11508 0.19037 

> summary(b1)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-1.524732 -0.003446  0.300391  0.302333  0.606988  2.426530 
> summary(b2)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-1.72873 -0.00948  0.29729  0.29661  0.60247  2.11381 
> summary(b3)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05054 0.25741 0.30038 0.30017 0.34304 0.55739 

> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.3979  0.4393  0.4489  0.4490  0.4585  0.5128 
> summary(b2_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.3990  0.4392  0.4488  0.4491  0.4586  0.5102 
> summary(b3_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05595 0.06198 0.06334 0.06335 0.06467 0.07087 

> sd(b1)
[1] 0.452627
> sd(b2)
[1] 0.4529496
> sd(b3)
[1] 0.06325491

##################################################################################################################################################################
## Scenario 3: correlation of 0.99 between X1 and X2, X1 and X3, X2 and X3

library(MASS)
b1 <- b2 <- b3 <- NULL
b1_SE <- b2_SE <- b3_SE <- NULL
r2 <- NULL
for(i in 1:10000){
  print(i)
  Sigma <- matrix(c(1, 0.99, 0.99, 
                    0.99, 1, 0.99, 
                    0.99, 0.99, 1), 3, 3)
  help <- mvrnorm(n = 1000, mu = c(0, 0, 0), Sigma = Sigma, tol = 1e-6, empirical = FALSE)
  X1 <- help[,1]
  X2 <- help[,2]
  X3 <- help[,3]
  Y	<- 0.3*X1 + 0.3*X2 + 0.3*X3 + rnorm(1000,mean=0,sd=2)	
  help <- summary(lm(Y~X1+X2+X3))
  r2[i] <- help$r.squared
  b1[i]    <- help$coefficients[2,1]
  b2[i]    <- help$coefficients[3,1]
  b3[i]    <- help$coefficients[4,1]
  b1_SE[i] <- help$coefficients[2,2]
  b2_SE[i] <- help$coefficients[3,2]
  b3_SE[i] <- help$coefficients[4,2]
}

> summary(r2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.09626 0.15442 0.16939 0.16965 0.18419 0.25599 

> summary(b1)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-1.51732 -0.04877  0.30589  0.30577  0.66098  2.48433 
> summary(b2)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-1.59417 -0.04546  0.29640  0.29781  0.64397  1.98024 
> summary(b3)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-1.63234 -0.05271  0.29596  0.29607  0.64311  2.49427 

> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.4565  0.5066  0.5175  0.5178  0.5288  0.5843 
> summary(b2_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.4534  0.5064  0.5176  0.5179  0.5290  0.5819 
> summary(b3_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.4606  0.5061  0.5174  0.5176  0.5288  0.5969 

> sd(b1)
[1] 0.5240749
> sd(b2)
[1] 0.5161041
> sd(b3)
[1] 0.5130649

##################################################################################################################################################################
## Scenario 4: no correlation between X1 and X2, but Y has been generated from joint multivariate normal distribution with X1, X2 (not conditioning on X2, X2)

library(MASS)
b1 <- b2 <- NULL
b1_SE <- b2_SE <- NULL
r2 <- NULL
for(i in 1:10000){
  print(i)
  Sigma <- matrix(c(1, 0, 0.3, 
                    0, 1, 0.3, 
                    0.3, 0.3, 1), 3, 3)
  help <- mvrnorm(n = 1000, mu = c(0, 0, 0), Sigma = Sigma, tol = 1e-6, empirical = FALSE)
  X1 <- help[,1]
  X2 <- help[,2]
  Y <- help[,3]
  help <- summary(lm(Y~X1+X2))
  r2[i] <- help$r.squared
  b1[i]    <- help$coefficients[2,1]
  b2[i]    <- help$coefficients[3,1]
  b1_SE[i] <- help$coefficients[2,2]
  b2_SE[i] <- help$coefficients[3,2]
}
> summary(r2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.1039  0.1660  0.1803  0.1809  0.1955  0.2714 

> summary(b1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.1802  0.2801  0.3000  0.2999  0.3197  0.3980 
> summary(b2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.1962  0.2798  0.2993  0.2991  0.3185  0.4127 

> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02538 0.02805 0.02866 0.02867 0.02927 0.03221 
> summary(b2_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02548 0.02805 0.02867 0.02867 0.02929 0.03199 

> sd(b1)
[1] 0.02874199
> sd(b2)
[1] 0.02888573

##################################################################################################################################################################
## Scenario 5: correlation of 0.99 between X1 and X2
##             Y has been generated from joint multivariate normal distribution with X1, X2 (not conditioning on X2, X2)

library(MASS)
b1 <- b2 <- NULL
b1_SE <- b2_SE <- NULL
r2 <- NULL
for(i in 1:10000){
  print(i)
  Sigma <- matrix(c(1, 0.99, 0.3, 
                    0.99, 1, 0.3, 
                    0.3, 0.3, 1), 3, 3)
  help <- mvrnorm(n = 1000, mu = c(0, 0, 0), Sigma = Sigma, tol = 1e-6, empirical = FALSE)
  X1 <- help[,1]
  X2 <- help[,2]
  Y <- help[,3]
  help <- summary(lm(Y~X1+X2))
  r2[i] <- help$r.squared
  b1[i]    <- help$coefficients[2,1]
  b2[i]    <- help$coefficients[3,1]
  b1_SE[i] <- help$coefficients[2,2]
  b2_SE[i] <- help$coefficients[3,2]
}

> summary(r2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.03357 0.07933 0.09114 0.09169 0.10306 0.18062 

> summary(b1)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-0.666595  0.004679  0.145244  0.147775  0.295067  1.014169 
> summary(b2)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-0.663541  0.005946  0.156442  0.152963  0.298050  0.934519 

> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.1903  0.2095  0.2140  0.2141  0.2186  0.2441 
> summary(b2_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.1890  0.2094  0.2140  0.2141  0.2186  0.2433 

> sd(b1)
[1] 0.2144935
> sd(b2)
[1] 0.2145617

##################################################################################################################################################################
### Direct comparison to show why R^2 is larger under multicollinearity:

library(MASS)
b11 <- r21 <- mean1 <- var1 <- SSE1 <- SS_tot1 <- SS_reg1 <- NULL
for(i in 1:10000){
  print(i)
  Sigma <- matrix(c(1, 0.4, 0.4, 
                    0.4, 1, 0.4, 
                    0.4, 0.4, 1), 3, 3)
  help <- mvrnorm(n = 1000, mu = c(0, 0, 0), Sigma = Sigma, tol = 1e-6, empirical = FALSE)
  X1 <- help[,1]
  X2 <- help[,2]
  X3 <- help[,3]
  Y	<- 0.3*X1 + 0.3*X2 + 0.3*X3 + rnorm(1000,mean=0,sd=2)	
  mean1[i] <- mean(Y)
  var1[i] <- var(Y)
  help <- lm(Y~X1+X2+X3)
  SS_tot1[i] <- sum((Y-mean(Y))^2)
  SS_reg1[i] <- sum((predict(help)-mean(Y))^2)
  r21[i]  <- summary(help)$r.squared
  b11[i]  <- summary(help)$coefficients[2,1]
  SSE1[i] <- sum((predict(help)-Y)^2)
}

b12 <- r22 <- mean2 <- var2 <- SSE2 <- SS_tot2 <- SS_reg2 <- NULL
for(i in 1:10000){
  print(i)
  Sigma <- matrix(c(1, 0, 0, 
                    0, 1, 0, 
                    0, 0, 1), 3, 3)
  help <- mvrnorm(n = 1000, mu = c(0, 0, 0), Sigma = Sigma, tol = 1e-6, empirical = FALSE)
  X1 <- help[,1]
  X2 <- help[,2]
  X3 <- help[,3]
  Y	<- 0.3*X1 + 0.3*X2 + 0.3*X3 + rnorm(1000,mean=0,sd=2)	
  mean2[i] <- mean(Y)
  var2[i] <- var(Y)
  help <- lm(Y~X1+X2+X3)
  SS_tot2[i] <- sum((Y-mean(Y))^2)
  SS_reg2[i] <- sum((predict(help)-mean(Y))^2)
  r22[i]  <- summary(help)$r.squared
  b12[i]  <- summary(help)$coefficients[2,1]
  SSE2[i] <- sum((predict(help)-Y)^2)
  #r2 = SS_reg2 / SS_tot2
}

> summary(mean1)
Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
-0.2560114 -0.0445057  0.0001941 -0.0002579  0.0433158  0.2501306 
> summary(mean2)
Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
-0.2282019 -0.0436157 -0.0010949 -0.0001263  0.0436717  0.3370265 

> summary(var1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
3.713   4.353   4.487   4.488   4.623   5.250 
> summary(var2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
3.456   4.144   4.268   4.271   4.398   5.127 

> summary(SS_tot1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
3709    4349    4483    4484    4618    5245 
> summary(SS_tot2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
3453    4140    4264    4267    4393    5121 

> summary(SS_reg1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
189.5   433.2   492.1   497.1   555.4   980.1 
> summary(SS_reg2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
92.83  234.30  278.06  281.50  324.33  563.66 

> summary(r21)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.04878 0.09776 0.10996 0.11067 0.12285 0.19740 
> summary(r22)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02176 0.05549 0.06517 0.06585 0.07533 0.13133 

> summary(b11)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.03985 0.25159 0.29967 0.30018 0.34985 0.56039 
> summary(b12)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.06801 0.25792 0.29954 0.29993 0.34240 0.55780 

> summary(SSE1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
3330    3869    3985    3987    4107    4683 
> summary(SSE2)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
3312    3865    3984    3986    4105    4729 
 
> anova(help)
Analysis of Variance Table

Response: Y
Df Sum Sq Mean Sq F value    Pr(>F)    
X1          1   28.7  28.733   6.715    0.0097 ** 
  X2          1  115.2 115.176  26.917 2.573e-07 ***
  X3          1   91.8  91.762  21.445 4.123e-06 ***
  Residuals 996 4261.9   4.279                      
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# SSE: SS of residuals row
# SS_reg: sum of SS of predictor rows
# SS_tot: sum of SSE and SS_reg