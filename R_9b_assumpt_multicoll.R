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

# -> all good!

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

# -> point estimates still all good, and SE also similar to actual ones, but much larger for X1 and X2, hence power loss.

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

# -> point estimates still all good, and SE also similar to actual ones, but much larger, hence power loss.
