#######################Q1#######################

mydata <- read.csv("C:/Users/Ana/Documents/LiU University/Machine Learning/csv data for labs/Lab 1 data/pima-indians-diabetes.csv")

#Creating scatterplot
plot(x = mydata[, 8], 
     y = mydata[, 2],
     xlab = "Age",
     ylab = "Plasma glucose concentration",
     main = "Age vs Plasma glucose concentration",
     col = as.factor(mydata[, 9]))

#######################Q2#######################

#Separating into train/test data

X = mydata[, c(2, 8)]  
y = mydata[, 9]        


n = dim(mydata)[1]    
set.seed(12345)        
id = sample(1:n, floor(n*0.7))  

X_train = X[id, ] 
colnames(X_train) = c("Glucose", "Age")
y_train = y[id]        
X_test = X[-id, ]  
X_test = as.data.frame(X_test)
colnames(X_test) = c("Glucose", "Age")
y_test = y[-id]        

train_data = data.frame(Glucose = X_train[, 1], Age = X_train[, 2], Diabetes = y_train)

#Training logistic regression model
logit_model = glm(y_train ~ Glucose + Age,data = train_data, family = binomial)
#x_1 is Plasma glucose
#x_2 is Age
summary(logit_model)

# Extract coefficients
intercept = coef(logit_model)[1]
coef_x1 = coef(logit_model)[2]  
coef_x2 = coef(logit_model)[3]  

# Probabilistic equation
cat("P(y = 1 | x1, x2) = 1 / (1 + exp(-(", intercept, "+", coef_x1, "* x1 +", coef_x2, "* x2)))\n")

# Predict probabilities and classify for training set
train_probs = predict(logit_model, newdata = data.frame(Glucose = X_train[, 1], Age = X_train[, 2]), type = "response")
train_preds = ifelse(train_probs >= 0.5, 1, 0)
train_error = mean(train_preds != y_train)
cat("Training Misclassification Error:", train_error, "\n")


# Predict probabilities and classify for test set
test_probs = predict(logit_model, newdata = X_test, type = "response")
test_preds = ifelse(test_probs >= 0.5, 1, 0)

#Creating new, predict scatterplot
plot(x = X_test[,2], 
     y = X_test[,1],
     xlab = "Age",
     ylab = "Plasma glucose concentration",
     main = "Predicted Diabetes",
     col = as.factor(test_preds))

#######################Q3#######################
decision_boundary <- function(x_1) {
  -(intercept/coef_x1) - (coef_x2/coef_x1) * x_1}

curve(decision_boundary(x), add = TRUE, col = "blue", lwd = 2)

#######################Q4#######################

#  r = 0.2

# Predict probabilities and classify for training set
train_probs = predict(logit_model, newdata = data.frame(Glucose = X_train[, 1], Age = X_train[, 2]), type = "response")
train_preds = ifelse(train_probs >= 0.2, 1, 0)
train_error = mean(train_preds != y_train)
cat("Training Misclassification Error:", train_error, "\n")


# Predict probabilities and classify for test set
test_probs = predict(logit_model, newdata = X_test, type = "response")
test_preds = ifelse(test_probs >= 0.2, 1, 0)

#Creating new, predict scatterplot
plot(x = X_test[,2], 
     y = X_test[,1],
     xlab = "Age",
     ylab = "Plasma glucose concentration",
     main = "Predicted Diabetes",
     col = as.factor(test_preds))



#  r = 0.8

# Predict probabilities and classify for training set
train_probs = predict(logit_model, newdata = data.frame(Glucose = X_train[, 1], Age = X_train[, 2]), type = "response")
train_preds = ifelse(train_probs >= 0.8, 1, 0)
train_error = mean(train_preds != y_train)
cat("Training Misclassification Error:", train_error, "\n")


# Predict probabilities and classify for test set
test_probs = predict(logit_model, newdata = X_test, type = "response")
test_preds = ifelse(test_probs >= 0.8, 1, 0)

#Creating new, predict scatterplot
plot(x = X_test[,2], 
     y = X_test[,1],
     xlab = "Age",
     ylab = "Plasma glucose concentration",
     main = "Predicted Diabetes",
     col = as.factor(test_preds))

