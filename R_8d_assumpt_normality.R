##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
### Aim: Check the violation of the normal assumption of residuals in the regression

##################################################################################################################################################################
## Scenario 1: normally-distributed residuals

b1 <- b1_SE <- b1_pval <- NULL

for(i in 1:10000){
  print(i)
  X1 <- rnorm(1000, mean=0, sd=1)
  Y	<- 0.3*X1 + rnorm(1000,mean=0,sd=1)	
  help <- summary(lm(Y~X1))$coefficients
  b1[i]      <- help[2,1]
  b1_SE[i]   <- help[2,2]
  b1_pval[i] <- help[2,4]
}

> summary(b1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.1816  0.2794  0.3010  0.3008  0.3223  0.4235 
> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02832 0.03098 0.03166 0.03167 0.03236 0.03592 
> sd(b1)
[1] 0.03155079
> summary(b1_pval)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.000e+00 0.000e+00 0.000e+00 2.079e-12 0.000e+00 4.657e-09 

##################################################################################################################################################################
## Scenario 2: t-distributed residuals

b1 <- b1_SE <- b1_pval <- NULL

for(i in 1:10000){
  print(i)
  X1 <- rnorm(1000, mean=0, sd=1)
  Y	<- 0.3*X1 + rt(1000,df=5)	
  help <- summary(lm(Y~X1))$coefficients
  b1[i]      <- help[2,1]
  b1_SE[i]   <- help[2,2]
  b1_pval[i] <- help[2,4]
}

> summary(b1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.1322  0.2733  0.2998  0.3001  0.3273  0.4574 
> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.03525 0.03952 0.04069 0.04082 0.04199 0.06166 
> sd(b1)
[1] 0.04072486
> summary(b1_pval)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.000e+00 0.000e+00 0.000e+00 4.194e-07 0.000e+00 2.186e-03 

##################################################################################################################################################################
## Scenario 3: log-normally-distributed residuals

b1 <- b1_SE <- b1_pval <- NULL

for(i in 1:10000){
  print(i)
  X1 <- rnorm(1000, mean=0, sd=1)
  Y	<- 0.3*X1 + rlnorm(1000, meanlog = 0, sdlog = 1)
  help <- summary(lm(Y~X1))$coefficients
  b1[i]      <- help[2,1]
  b1_SE[i]   <- help[2,2]
  b1_pval[i] <- help[2,4]
}

> summary(b1)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.008511 0.256252 0.301680 0.301164 0.347569 0.625763 
> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.04648 0.06153 0.06631 0.06769 0.07184 0.25243 
> sd(b1)
[1] 0.06870369
> summary(b1_pval)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0000000 0.0000001 0.0000065 0.0033221 0.0001753 0.9242758 

##################################################################################################################################################################
## Scenario 4: t-distributed residuals

b1 <- b1_SE <- b1_pval <- NULL

for(i in 1:10000){
  print(i)
  X1 <- rnorm(1000, mean=0, sd=1)
  Y	<- 0.3*X1 + rt(1000,df=2)	
  help <- summary(lm(Y~X1))$coefficients
  b1[i]      <- help[2,1]
  b1_SE[i]   <- help[2,2]
  b1_pval[i] <- help[2,4]
}

> summary(b1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-1.5673  0.2383  0.2986  0.2990  0.3597  1.7707 
> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05693 0.07639 0.08586 0.09983 0.10263 1.83220 
> sd(b1)
[1] 0.1163073
> summary(b1_pval)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0000000 0.0000252 0.0007052 0.0396587 0.0115229 0.9977028 

##################################################################################################################################################################
## Scenario 5: t-distributed residuals

b1 <- b1_SE <- b1_pval <- NULL

for(i in 1:10000){
  print(i)
  X1 <- rnorm(1000, mean=0, sd=1)
  Y	<- 0.3*X1 + rt(1000,df=1)	
  help <- summary(lm(Y~X1))$coefficients
  b1[i]      <- help[2,1]
  b1_SE[i]   <- help[2,2]
  b1_pval[i] <- help[2,4]
}

> summary(b1)
Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
-1750.4654    -0.5127     0.2796     0.0641     1.1031   604.1401 
> summary(b1_SE)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.2062    0.7024    1.1898    4.9221    2.5064 2661.1472 
> sd(b1)
[1] 27.22446
> summary(b1_pval)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
0.0000111 0.2271556 0.4727155 0.4816290 0.7353552 0.9997670 

# possible further distributions:
# rexp(n, rate=10000)
# rexp(n, rate=0.0001)
# rcauchy(n, location = 0, scale = 1)