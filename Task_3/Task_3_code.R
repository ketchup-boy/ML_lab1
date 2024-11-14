#######################Q1#######################

mydata <- read.csv("C:/Users/Ana/Documents/LiU University/Machine Learning/csv data for labs/Lab 1 data/pima-indians-diabetes.csv")

plot(x = mydata$X50, 
     y = mydata$X148,
     xlab = "Age",
     ylab = "Plasma glucose concentration",
     main = "Age vs Plasma glucose concentration",
     col = as.factor(mydata$X1))
# legend("topright",                     
#        legend = levels(as.factor(mydata$X1)), 
#        col = 1:length(levels(as.factor(mydata$X1))))

