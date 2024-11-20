 # install.packages("readr")
 # install.packages("kknn")
 library(kknn)
 library(tidyverse)
data <- read_csv("Task_1/optdigits.csv")

#renamed for simplicity
colnames(data)[ncol(data)] <- "digit"
data$digit <- factor(data$digit)
#divide data for training, validation and testing
n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

# Fit the knn model


# Predict on training data
knn_model <- kknn(digit ~ ., train = train, test = train, k = 30, kernel = "rectangular")
train_pred <- fitted(knn_model)

# Predict on test data
test_knn_model <- kknn(digit ~ ., train = train, test = test, k = 30, kernel = "rectangular")
test_pred <- fitted(test_knn_model)

#confusion matrices
train_conf_matrix <- table(train$digit, train_pred)
print("Training Confusion Matrix:")
print(train_conf_matrix)

test_conf_matrix <- table(test$digit, test_pred)
print("Test Confusion Matrix:")
print(test_conf_matrix)

# Misclassification error
train_misclassification <- sum(train$digit != train_pred) / nrow(train)
print(paste("Training Misclassification Error:", round(train_misclassification, 4)))

test_misclassification <- sum(test$digit != test_pred) / nrow(test)
print(paste("Test Misclassification Error:", round(test_misclassification, 4)))

# Misclassification for each digit:
train_correctly_classified <- diag(as.matrix(train_conf_matrix))
test_correctly_classified <- diag(as.matrix(test_conf_matrix))

error_rate_digit_train <- 1 - (train_correctly_classified/rowSums(as.matrix(train_conf_matrix)))
error_rate_digit_test <- 1 - (test_correctly_classified/rowSums(as.matrix(test_conf_matrix)))

print(error_rate_digit_train)
print(error_rate_digit_test)

####################################################

# # Extract predicted probabilities and identify cases for digit "8"
train_probs <- knn_model$prob
# 
# Filter for only rows where the true digit is "8"
eight_indices <- which(train$digit == 8)
# 
# # Get probabilities of the correct class ("8") for these cases as a numeric vector
eight_probs <- vapply(eight_indices, function(i) as.numeric(train_probs[i, "8"]), numeric(1))

# Find indices of the 2 highest and 3 lowest probabilities
easiest_cases <- order(eight_probs, decreasing = TRUE)[1:2]
hardest_cases <- order(eight_probs)[1:3]

easiest_indexes <- eight_indices[easiest_cases]
hardest_indexes <- eight_indices[hardest_cases]

# Function to visualize a digit as an 8x8 heatmap
visualize_digit <- function(data_row) {
  matrix_data <- matrix(as.numeric(data_row[1:64]), nrow = 8, ncol = 8, byrow = TRUE)
  heatmap(matrix_data, Colv = NA, Rowv = NA, scale = "none",
           col = heat.colors(16))
}

# Visualize easiest cases
cat("Easiest cases of digit '8':\n")
for (i in easiest_indexes) {
  visualize_digit(train[i,])
}

# Visualize hardest cases
cat("Hardest cases of digit '8':\n")
for (i in hardest_indexes) {
  visualize_digit(train[i,])
}

######################################################


# Create storage for errors
train_errors <- numeric(30)  # Training error for each K
valid_errors <- numeric(30)  # Validation error for each K

# Loop over K = 1 to 30
for (k in 1:30) {
  # Fit k-NN model
  knn_model <- kknn(digit ~ ., train = train, test = valid, k = k, kernel = "rectangular")
  
  # Training predictions and error
  train_pred <- fitted(kknn(digit ~ ., train = train, test = train, k = k, kernel = "rectangular"))
  train_errors[k] <- mean(train_pred != train$digit)
  
  # Validation predictions and error
  valid_pred <- fitted(knn_model)
  valid_errors[k] <- mean(valid_pred != valid$digit)
}

# Plot the errors
# plot(1:30, train_errors, type = "o", col = "blue", ylim = c(0, max(train_errors, valid_errors)), 
#      xlab = "K", ylab = "Misclassification Error", main = "Training and Validation Errors vs K")
# lines(1:30, valid_errors, type = "o", col = "red")
# legend("topright", legend = c("Training Error", "Validation Error"), col = c("blue", "red"), lty = 1, pch = 1)
par(xpd = TRUE, mar = c(5, 4, 4, 7))  # Increase the right margin
plot(1:30, train_errors, type = "o", col = "blue", ylim = c(0, max(train_errors, valid_errors)), 
     xlab = "K", ylab = "Misclassification Error", main = "Training and Validation Errors vs K")
lines(1:30, valid_errors, type = "o", col = "red")
legend("bottomright", inset = c(-0.2, 0), legend = c("Training Error", "Validation Error"), 
       col = c("blue", "red"), lty = 1, pch = 1, cex = 0.8)

# Find optimal K (minimizing validation error)
optimal_k <- which.min(valid_errors)
cat("Optimal K:", optimal_k, "\n")

# Estimate the test error for the optimal K
knn_test <- kknn(digit ~ ., train = train, test = test, k = optimal_k, kernel = "rectangular")
test_pred <- fitted(knn_test)
test_error <- mean(test_pred != test$digit)
cat("Optimal K:", optimal_k, "\n")
cat("Test Error for optimal K:", test_error, "\n")

###########################################################################

max_k <- 30
cross_entropy_losses <- numeric(max_k)
epsilon <- 1e-15

for (k in 1:max_k) {
  # Fit k-NN model
  knn_model <- kknn(digit ~ ., train = train, test = valid, k = k, kernel = "rectangular")
  # Extract predicted probabilities
  #probs <- attributes(fitted(knn_model))$prob
  probs <- knn_model$prob
  
  # Ensure probabilities are retrieved properly
  if (is.null(probs)) {
    stop("Predicted probabilities not found. Check the model configuration.")
  }
  
  # Compute cross-entropy loss for validation data
  cross_entropy_loss <- 0
  for (i in 1:nrow(valid)) {
    #true_class <- as.numeric(valid$digit[i]) + 1  # Convert digit to 1-based index
    true_class <- as.numeric(valid$digit[i])
    predicted_prob <- probs[i, true_class]  # Get the probability for the true class
    cross_entropy_loss <- cross_entropy_loss - log(predicted_prob + epsilon)
  }
  cross_entropy_losses[k] <- cross_entropy_loss / nrow(valid)  # Normalize by number of samples
}
#dev.new(width = 8, height = 6)
# Plot cross-entropy losses
plot(1:max_k, cross_entropy_losses, type = "o", col = "red", 
     xlab = "K", ylab = "Cross-Entropy Loss", ylim = c(0, max(cross_entropy_losses)),
     main = "Cross-Entropy Loss vs K")

# Find the optimal K
optimal_k <- which.min(cross_entropy_losses)
cat("Optimal K (based on cross-entropy loss):", optimal_k, "\n")

##################################


