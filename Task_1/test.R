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


max_k <- 30
cross_entropy_losses <- numeric(max_k)  # Store cross-entropy loss for each K
epsilon <- 1e-15  # Small constant to avoid log(0)

# Loop over K = 1 to max_k
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