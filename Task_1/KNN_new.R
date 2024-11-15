# Load the necessary library
#library(kknn)

# Import data
#data <- read.csv("optdigits.csv", header = FALSE)

# Rename the last column as "digit" for clarity
colnames(data)[ncol(data)] <- "digit"
data$digit <- as.factor(data$digit) # Ensure the target is treated as a factor

# Set a seed for reproducibility
set.seed(123)

# Split data into training (50%), validation (25%), and test (25%) sets
train_idx <- sample(seq_len(nrow(data)), size = 0.5 * nrow(data))
train <- data[train_idx, ]
temp <- data[-train_idx, ]
val_idx <- sample(seq_len(nrow(temp)), size = 0.5 * nrow(temp))
validation <- temp[val_idx, ]
test <- temp[-val_idx, ]






# Fit 30-NN model with rectangular kernel on training data
knn_train <- kknn(digit ~ ., train = train, test = train, k = 30, kernel = "rectangular")
knn_test <- kknn(digit ~ ., train = train, test = test, k = 30, kernel = "rectangular")

# Confusion matrices
train_conf_mat <- table(Predicted = fitted(knn_train), Actual = train$digit)
test_conf_mat <- table(Predicted = fitted(knn_test), Actual = test$digit)

# Misclassification errors
train_error <- 1 - sum(diag(train_conf_mat)) / sum(train_conf_mat)
test_error <- 1 - sum(diag(test_conf_mat)) / sum(test_conf_mat)

train_error
test_error






# Extract predicted probabilities for digit "8"
train_probs <- as.data.frame(fitted(knn_train))
eight_probs <- train_probs[train$digit == "8", ]

# Sort to find highest and lowest probabilities for correct classification
highest_probs <- eight_probs[order(-eight_probs[["8"]])[1:2], ]
lowest_probs <- eight_probs[order(eight_probs[["8"]])[1:3], ]

# Function to plot 8x8 digit image
plot_digit <- function(row_data, title) {
  digit_matrix <- matrix(as.numeric(row_data[1:64]), nrow = 8, byrow = TRUE)
  heatmap(digit_matrix, Rowv = NA, Colv = NA, scale = "none", main = title)
}

# Plotting the easiest cases
plot_digit(train[train$digit == "8", ][row.names(highest_probs)[1], ], "Easiest Case 1 for 8")
plot_digit(train[train$digit == "8", ][row.names(highest_probs)[2], ], "Easiest Case 2 for 8")

# Plotting the hardest cases
plot_digit(train[train$digit == "8", ][row.names(lowest_probs)[1], ], "Hardest Case 1 for 8")
plot_digit(train[train$digit == "8", ][row.names(lowest_probs)[2], ], "Hardest Case 2 for 8")
plot_digit(train[train$digit == "8", ][row.names(lowest_probs)[3], ], "Hardest Case 3 for 8")






# Initialize vectors to store errors
train_errors <- numeric(30)
val_errors <- numeric(30)

# Loop over K values
for (k in 1:30) {
  knn_model <- kknn(digit ~ ., train = train, test = train, k = k, kernel = "rectangular")
  val_model <- kknn(digit ~ ., train = train, test = validation, k = k, kernel = "rectangular")
  
  # Calculate errors
  train_errors[k] <- 1 - sum(diag(table(fitted(knn_model), train$digit))) / nrow(train)
  val_errors[k] <- 1 - sum(diag(table(fitted(val_model), validation$digit))) / nrow(validation)
}

# Plot the results
plot(1:30, train_errors, type = "o", col = "blue", ylim = range(c(train_errors, val_errors)),
     xlab = "K", ylab = "Misclassification Error", main = "Training and Validation Errors")
lines(1:30, val_errors, type = "o", col = "red")
legend("topright", legend = c("Training Error", "Validation Error"), col = c("blue", "red"), lty = 1)

# Identify optimal K and evaluate on test set
optimal_k <- which.min(val_errors)
knn_opt_test <- kknn(digit ~ ., train = train, test = test, k = optimal_k, kernel = "rectangular")
test_opt_error <- 1 - sum(diag(table(fitted(knn_opt_test), test$digit))) / nrow(test)
optimal_k
test_opt_error








cross_entropy_errors <- numeric(30)

for (k in 1:30) {
  val_model <- kknn(digit ~ ., train = train, test = validation, k = k, kernel = "rectangular")
  probs <- as.data.frame(fitted(val_model))
  actual <- model.matrix(~ digit - 1, data = validation)
  
  # Calculate cross-entropy
  cross_entropy_errors[k] <- -sum(actual * log(probs + 1e-15)) / nrow(validation)
}

# Plot cross-entropy for validation data
plot(1:30, cross_entropy_errors, type = "o", col = "purple", xlab = "K", ylab = "Cross-Entropy Error",
     main = "Cross-Entropy on Validation Data")

# Optimal K for cross-entropy
optimal_k_ce <- which.min(cross_entropy_errors)
optimal_k_ce





