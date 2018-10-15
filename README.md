# Regression-Bootstrapping-Assignment

#QUESTION 1
#original code that produces a dataset that conforms to the classic univariate regression model. 
#data set should have 99 observations and a Normal error term.
set.seed(123)
N <- 99
x1 <- rbeta(50, shape1 = 2, shape2 = 3)
x2 <- rbeta(49, shape1 = 3, shape2 = 4)
x <- c(x1,x2)
y <- 5 + x*2 + rnorm(99,sd = 0.1)

data <- data.frame(x=x, y=y)
#original model
unreg_model <- lm(y~x)
#shows positive slope coefficient 
summary(unreg_model)

#plot of original 99 data points and original regression line
dataviz1 <- plot(data, xlab = "Predictor Variable", ylab = "Output Variable", main = "Question #1 Dataviz") 
axis(side = 2, at=c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50))

dataviz1 + abline(unreg_model, col = "blue", lwd = 2) 

#add an outlier as data point 100
data[nrow(data) + 1,] = c(0.1,45)
#not sure why, but kept getting a row with NA values
data2 <- na.omit(data)

#univariate regression model now has negative slope
unreg_model2 <- lm(data2$y ~ data2$x)
summary(unreg_model2)


#adding new regression model line to scatterplot
dataviz1 + abline(unreg_model2, col = "red", lw = 2) +
  text(x = 0.1 ,y = 40, labels = c("Outlier"))

#adding labels + caption to dataviz
dataviz1 + legend("topright", legend = c("Blue = Original Regression", "Red = Updated Regression")) 


#QUESTION 2
library("Matching")
library("ggplot2")
library("dplyr")
library("reshape2")
library(arm)
data('lalonde')

#use only data in control group (treat = 0)
lalonde <- lalonde[lalonde$treat == 0,]

#addative regression model
mylm <- lm(re78 ~ age + educ + re74 + re75 + educ*re74 + educ*re75 + age*re74 + 
             age*re75 + re74*re75, data = lalonde)

set.seed(123)
rounds <- 10000
simulation<- sim(mylm, n.sims = rounds)

nsamples <- length(lalonde$age)
re78preds_median <- matrix(0,nrow = rounds, ncol = nsamples)

for (i in 1:rounds) {
  re78preds_median[i,] <- 
    simulation@coef[i,1] + 
    simulation@coef[i,2]*lalonde$age +
    simulation@coef[i,3]*median(lalonde$educ)  +
    simulation@coef[i,4]*median(lalonde$re74) +
    simulation@coef[i,5]*median(lalonde$re75) +
    simulation@coef[i,6]*median(lalonde$educ)*median(lalonde$re74) +
    simulation@coef[i,7]*median(lalonde$educ)*median(lalonde$re75) +
    simulation@coef[i,8]*lalonde$age*median(lalonde$re74) +
    simulation@coef[i,9]*lalonde$age*median(lalonde$re75) +
    simulation@coef[i,10]*median(lalonde$re74)*median(lalonde$re75) +
    rnorm(nsamples, 0, simulation@sigma[i])
}


# Calculing prediction intervals for each age value
re78preds_median_intervals <- apply(re78preds_median, 2, quantile, c(0.025, 0.975))

#Creating a table frame for a table showing: elevant point estimates 
##(e.g., the bounds of the prediction intervals of y for the different ages, and the medians of the other predictors)
re78preds_median_results<- data.frame(age=lalonde$age, educ=median(lalonde$educ),  
           re75=median(lalonde$re75), re74=median(lalonde$re74), 
           qlower = re78preds_median_intervals[1,], 
           qupper = re78preds_median_intervals[2,])

##Resulting table is incredibly long; using head() to show the first 6 
head(re78preds_median_results)

#if want to reorder by age
re78preds_median_results <- re78preds_median_results[order(re78preds_median_results$age),]
head(re78preds_median_results)

#Scatter plot #1
melted <- melt(re78preds_median_results[,c('age', 'qlower', 'qupper')], id.vars = 'age')
plot1<- ggplot(melted, aes(x=age, y=value, color=variable)) + geom_point() + xlab('Age') + ylab('Confidence Interval of re78') +
  theme_minimal() 

plot1 + guides(color=guide_legend(title="Quantile")) + ggtitle("Predicting re78, Holding All Variables But Age at Their Median")


##second simulation: holding predictors at 90% 
re78preds_ninety <- matrix(0,nrow = rounds, ncol = nsamples)
for (i in 1:rounds) {
  re78preds_ninety[i,] <- 
    simulation@coef[i,1] + 
    simulation@coef[i,2]*lalonde$age +
    simulation@coef[i,3]*quantile(lalonde$educ, 0.9) +
    simulation@coef[i,4]*quantile(lalonde$re74, 0.9) +
    simulation@coef[i,5]*quantile(lalonde$re75, 0.9) +
    simulation@coef[i,6]*quantile(lalonde$educ, 0.9)*quantile(lalonde$re74, 0.9) +
    simulation@coef[i,7]*quantile(lalonde$educ, 0.9)*quantile(lalonde$re75, 0.9) +
    simulation@coef[i,8]*lalonde$age*quantile(lalonde$re74, 0.9) +
    simulation@coef[i,9]*lalonde$age*quantile(lalonde$re75, 0.9) +
    simulation@coef[i,10]*quantile(lalonde$re74, 0.9)*quantile(lalonde$re75, 0.9) +
    rnorm(nsamples, 0, simulation@sigma[i])
}

# Calculing prediction intervals for each age value
re78preds_ninety_intervals <- apply(re78preds_ninety, 2, quantile, c(0.025, 0.975))

#Contructing data frame to present results in table
re78preds_ninety_results <- data.frame(age=lalonde$age, educ=unname(quantile(lalonde$educ, 0.9)),  
           re75=unname(quantile(lalonde$re75, 0.9)), re74=unname(quantile(lalonde$re74, 0.9)), 
           qlower = re78preds_ninety_intervals[1,], 
           qupper = re78preds_ninety_intervals[2,])

##if want to reorder by age
re78preds_ninety_results <- re78preds_ninety_results[order(re78preds_ninety_results$age),]

head(re78preds_ninety_results)

#Scatter Plot #2
melted <- melt(re78preds_ninety_results[,c('age', 'qlower', 'qupper')], id.vars = 'age')
plot2 <- ggplot(melted, aes(x=age, y=value, color=variable)) + geom_point() + xlab('Age') + ylab('Confidence Interval of re78') +
  theme_minimal()

plot2 + guides(color=guide_legend(title="Quantile")) + ggtitle("Predicting re78, Holding All Variables But Age at Their 90th Percentile")

#Question 3
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
#NOTE: This method produced slightly different upper/lower bounds compared to the manually 
## written analytic and simulated methods in Approach #1

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

#QUESTION 4
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


#QUESTION 5
#logistic regression model should be a linear additive 
##function of all predictors available to you -- no interaction terms needed; no use of re78 as is post-treat

mylm <- lm(treat ~ age + education + black + hispanic + married + nodegree + re75, data = mydata)

#create a new data fram
assignment.prob <- data.frame(mydata, Treatment_Assignment_Probablity = mylm$fitted.values)

#subset treatment & control groups to plot histograms seperately
treat<- subset(assignment.prob, treat == 1, select = Treatment_Assignment_Probablity)
control <- subset(assignment.prob, treat == 0, select = Treatment_Assignment_Probablity)

#plot histograms for comparison; treatment in red, control group in blue
par(mfrow=c(1,2))
hist(treat$Treatment_Assignment_Probablity, main = "Treatment Group", xlab = "Estimated Probability", col = "red")
hist(control$Treatment_Assignment_Probablity, main = "Control Group", xlab = "Estimated Probability", col = "blue")


           



