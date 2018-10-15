library(foreign)
mydata <- read.dta(file = "nsw.dta")

#logistic regression model should be a linear additive 
##function of all predictors available to you -- no interaction terms needed; no use of re78 as is post-treat
mylm <- lm(treat ~ age + education + black + hispanic + married + nodegree + re75, data = mydata)
assignment.prob <- data.frame(mydata, Treatment_Assignment_Probablity = mylm$fitted.values)

treat<- subset(assignment.prob, treat == 1, select = Treatment_Assignment_Probablity)
control <- subset(assignment.prob, treat == 0, select = Treatment_Assignment_Probablity)

par(mfrow=c(1,2))
hist(treat$Treatment_Assignment_Probablity, main = "Treatment Group", xlab = "Estimated Probability", col = "red")
hist(control$Treatment_Assignment_Probablity, main = "Control Group", xlab = "Estimated Probability", col = "blue")
