#original code that produces a dataset that conforms to the classic univariate regression model. 
#Your data set should have 99 observations and a Normal error term.
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



