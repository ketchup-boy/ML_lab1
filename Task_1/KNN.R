install.packages("readr")
install.packages("kknn")
library(kknn)

# Load the data
data <- read_csv("Task_1/optdigits.csv")

#renamed for simplicity
colnames(data)[ncol(data)] <- "digit"

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
knn_model <- kknn(digit ~ ., train = train, test = train, k = 30, kernel = "rectangular")


# Predict on training data
train_pred <- fitted(knn_model)

# Predict on test data
test_knn_model <- kknn(digit ~ ., train = train, test = test, k = 30, kernel = "rectangular")
test_pred <- fitted(test_knn_model)

#round these bad boys
train_pred <- as.integer(round(train_pred))
test_pred <- as.integer(round(test_pred))

#get confusion matrices
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




# Extract predicted probabilities and identify cases for digit "8"
train_probs <- attributes(fitted(knn_model))$prob

# Filter for only rows where the true digit is "8"
eight_indices <- which(train$digit == 8)

# Get probabilities of the correct class ("8") for these cases as a numeric vector
eight_probs <- vapply(eight_indices, function(i) as.numeric(train_probs[i, "8"]), numeric(1))

# Find indices of the 2 highest and 3 lowest probabilities
easiest_cases <- eight_indices[order(eight_probs, decreasing = TRUE)[1:2]]
hardest_cases <- eight_indices[order(eight_probs)[1:3]]




# Function to visualize a digit as an 8x8 heatmap
visualize_digit <- function(data_row) {
  matrix_data <- matrix(as.numeric(data_row[1:64]), nrow = 8, ncol = 8, byrow = TRUE)
  heatmap(matrix_data, Colv = NA, Rowv = NA, scale = "none", 
          main = paste("Digit:", data_row[65]), col = heat.colors(16))
}

# Visualize easiest cases
cat("Easiest cases of digit '8':\n")
for (i in easiest_cases) {
  visualize_digit(train[i, ])
}

# Visualize hardest cases
cat("Hardest cases of digit '8':\n")
for (i in hardest_cases) {
  visualize_digit(train[i, ])
}

