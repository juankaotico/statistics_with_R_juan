##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
### Aim: Check the assumption on homoscedastic residuals

##################################################################################################################################################################
## Scenario 1: homoscedastic residuals

## a) when there is an effect of X1 on Y:
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

# Let's look at the estimates of b1, they should be 0.3
> summary(b1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.1816  0.2794  0.3010  0.3008  0.3223  0.4235 
# -> yes, they are on average 0.3!
> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02832 0.03098 0.03166 0.03167 0.03236 0.03592 
> sd(b1)
[1] 0.03155079
# -> the estimates SE of the b1 estimates are on average very similar to the empirical SE.

## b) when there is no effect of X1 on Y:
b1 <- b1_SE <- b1_pval <- NULL
for(i in 1:10000){
  print(i)
  X1 <- rnorm(1000, mean=0, sd=1)
  Y	<- 0*X1 + rnorm(1000,mean=0,sd=1)	
  help <- summary(lm(Y~X1))$coefficients
  b1[i]      <- help[2,1]
  b1_SE[i]   <- help[2,2]
  b1_pval[i] <- help[2,4]
}
> summary(b1)
Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
-0.1192751 -0.0207794  0.0003654  0.0004263  0.0211163  0.1589592 
> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02799 0.03100 0.03166 0.03168 0.03235 0.03578 
> sd(b1)
[1] 0.03153868
> table(b1_pval < 0.05)
FALSE  TRUE 
9496   504 
# -> same as above, and as it should be, about 5% false positives

##################################################################################################################################################################
## Scenario 2: heteroscedastic residuals

## a) when there is an effect of X1 on Y:
b1 <- b1_SE <- b1_pval <- NULL
for(i in 1:10000){
  print(i)
  X1 <- rnorm(1000, mean=0, sd=1)
  Y	<- 0.3*X1 + X1*rnorm(1000,mean=0,sd=1)	
  help <- lm(Y~X1)
  b1[i]      <- summary(help)$coefficients[2,1]
  b1_SE[i]   <- summary(help)$coefficients[2,2]
  b1_pval[i] <- summary(help)$coefficients[2,4]
}

> summary(b1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.07376 0.26444 0.30008 0.30045 0.33709 0.50639
# -> still unbiased on average
> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02736 0.03072 0.03154 0.03156 0.03237 0.03776 
> sd(b1)
[1] 0.05456796
# -> the estimates SE of the b1 estimates are on average much smaller than the empirical SE.

## b) when there is no effect of X1 on Y:
b1 <- b1_SE <- b1_pval <- NULL
for(i in 1:10000){
  print(i)
  X1 <- rnorm(1000, mean=0, sd=1)
  Y	<- 0*X1 + X1*rnorm(1000,mean=0,sd=1)	
  help <- lm(Y~X1)
  b1[i]      <- summary(help)$coefficients[2,1]
  b1_SE[i]   <- summary(help)$coefficients[2,2]
  b1_pval[i] <- summary(help)$coefficients[2,4]
}

> summary(b1)
Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
-0.2268932 -0.0376376 -0.0007377 -0.0003276  0.0368402  0.2294955 
> summary(b1_SE)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.02732 0.03075 0.03155 0.03157 0.03237 0.03707 
> sd(b1)
[1] 0.05533487
> table(b1_pval < 0.05)
FALSE  TRUE 
7338  2662 
# -> many false positives since actual SE is underestimated!