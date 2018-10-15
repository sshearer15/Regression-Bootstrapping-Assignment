##Question 3
library(foreign)
library(boot)
mydata <- read.dta(file = "nsw.dta")

##Approach 1
#regression models based on treatment
mylm <- lm(re78 ~ treat, data = mydata)
summary(mylm)

#simulated bootstrapping, without using canned boot functions
treat_coeff <- rep(0,10000)
for (i in 1:10000){
  indx <- sample(nrow(mydata), nrow(mydata), replace = T)
  this_lm <- lm(re78 ~ treat, data = mydata[indx,])
  treat_coeff[i] <- coef(this_lm)[2]
}

#histogragram of boostrapped treatment coefficient estimates
hist(treat_coeff, main = "Histogram #1 (No 'Canned' Functions)", xlab = "Treatment Effect Coefficient Estimates")
#95% confidence intervals of boostraped estimates of treat coefficients
boot.treat.conf<-quantile(treat_coeff, c(0.025,0.975))
#table comparing analytic coefficient values to boostrapped version
data.frame(analytical=c(confint(mylm)[2,1],confint(mylm)[2,2]),
           simulated=unname(quantile(treat_coeff, c(0.025, 0.975))), 
           row.names = c('lower bound', 'upper bound'))

##Approach #2
###Alternative using bootstrap canned functions
fn.coef <- function(data, index) {
  lm.temp <- lm(re78 ~ treat, data = data[index, ])
  return(coef(lm.temp)[2])  # treatment effect 
}

# Bootstrap
boot.coef <- boot(mydata, fn.coef, R = 10000)

# Find simulated confidence intervals and compare with regression standard error
quantile(boot.coef$t, c(0.025, 0.975))
lm.nsw <- lm(re78 ~ treat, data=mydata)
confint(lm.nsw)[2,]

#Creating a data frame and table comparing different methods' estimates
df <- data.frame(regression = confint(lm.nsw)[2,], bootstrapped = quantile(boot.coef$t, c(0.025, 0.975)))
#histogram for approach #2
hist(boot.coef$t, main = "Histogram #2 (Using 'Canned' Functions)", 
     xlab = "Treatment Effect Coefficient Estimates")

#NOTE: This method produced slightly different upper/lower bounds compared to the manually 
## written analytic and simulated methods in Approach #1

##Question 4
pred.ys <- predict(mylm, newdata = mydata)

rsq <- function(y, pred.ys) {
  SSres <- sum((pred.ys- y)^2)
  SStot <- sum((mean(y)-y)^2)
  rsq <- (1 - SSres/SStot)
  return(rsq)
}

#Proof using function compared to R2 value shown in mylm printout
rsq(mydata$re78, pred.ys)
summary(mylm)

           
