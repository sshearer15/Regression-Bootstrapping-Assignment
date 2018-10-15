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
