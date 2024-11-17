library(randomForest)




x1<-runif(100)
x2<-runif(100)
trdata<-cbind(x1,x2)
y<-as.numeric(x1<x2)
trlabels<-as.factor(y)

x1_test<-runif(100)
x2_test<-runif(100)
trdata_test <-cbind(x1_test,x2_test)
y_test<-as.numeric(x1_test<x2_test)
trlabels_test <-as.factor(y_test)

plot(x1,x2,col=(y+1))

rf1 <- randomForest(factor(y) ~ ., data = trdata, ntree=1, nodesize = 25, keep.forest = TRUE)
rf10 <- randomForest(factor(y) ~ ., data = trdata, ntree=10, nodesize = 25, keep.forest = TRUE)
rf100 <- randomForest(factor(y) ~ ., data = trdata, ntree=100, nodesize = 25, keep.forest = TRUE)

pred_rf1 <- predict(rf1, newdata = trdata_test)
pred_rf10 <- predict(rf10, newdata = trdata_test)
pred_rf100 <- predict(rf100, newdata = trdata_test)

print(mean(pred_rf100 == trlabels_test))

head(pred_rf1)
head(pred_rf10)
head(pred_rf100)

table(pred_rf1, trlabels)
table(pred_rf10, trlabels)
table(pred_rf100, trlabels)

accuracies_1 <- c()
accuracies_10 <- c()
accuracies_100 <- c()

test_x1<-runif(100)
test_x2<-runif(100)
test_trdata<-cbind(test_x1,test_x2)
test_y<-as.numeric(test_x1<test_x2)
test_trlabels<-as.factor(test_y)

for(i in 1:1000){
  x1<-runif(100)
  x2<-runif(100)
  trdata<-cbind(x1,x2)
  y<-as.numeric(x1<x2)
  trlabels<-as.factor(y)
  rf1 <- randomForest(factor(y) ~ ., data = trdata, ntree=1, nodesize = 25, keep.forest = TRUE)
  rf10 <- randomForest(factor(y) ~ ., data = trdata, ntree=10, nodesize = 25, keep.forest = TRUE)
  rf100 <- randomForest(factor(y) ~ ., data = trdata, ntree=100, nodesize = 25, keep.forest = TRUE)
  
  pred_rf1 <- predict(rf1, newdata = test_trdata)
  accuracy_1 <- mean(pred_rf1 == test_y)
  accuracies_1 <- append(accuracies_1, accuracy_1)
  
  pred_rf10 <- predict(rf10, newdata = test_trdata)
  accuracy_10 <- mean(pred_rf10 == test_y)
  accuracies_10 <- append(accuracies_10, accuracy_10)
  
  pred_rf1 <- predict(rf100, newdata = test_trdata)
  accuracy_100 <- mean(pred_rf100 == test_y)
  accuracies_100 <- append(accuracies_100, accuracy_100)
}

print(mean(accuracies_1))
print(sd(accuracies_1))

print(mean(accuracies_10))
print(sd(accuracies_10))

print(mean(accuracies_100))
print(sd(accuracies_100))


##########################
  
  
test_x1<-runif(100)
test_x2<-runif(100)
test_trdata<-cbind(test_x1,test_x2)
test_y<-as.numeric(test_x1<0,5)
test_trlabels<-as.factor(test_y)


for(i in 1:1000){
  x1<-runif(100)
  x2<-runif(100)
  trdata<-cbind(x1,x2)
  y<-as.numeric(x1<0.5)
  trlabels<-as.factor(y)
  rf1 <- randomForest(factor(y) ~ ., data = trdata, ntree=1, nodesize = 25, keep.forest = TRUE)
  rf10 <- randomForest(factor(y) ~ ., data = trdata, ntree=10, nodesize = 25, keep.forest = TRUE)
  rf100 <- randomForest(factor(y) ~ ., data = trdata, ntree=100, nodesize = 25, keep.forest = TRUE)
  
  pred_rf1 <- predict(rf1, newdata = test_trdata)
  accuracy_1 <- mean(as.numeric(pred_rf1 == test_y))
  accuracies_1 <- append(accuracies_1, accuracy_1)
  
  pred_rf10 <- predict(rf10, newdata = test_trdata)
  accuracy_10 <- mean(as.numeric(pred_rf10 == test_y))
  accuracies_10 <- append(accuracies_10, accuracy_10)
  
  pred_rf1 <- predict(rf100, newdata = test_trdata)
  accuracy_100 <- mean(as.numeric(pred_rf100 == test_y))
  accuracies_100 <- append(accuracies_100, accuracy_100)
}

print(mean(accuracies_1))
print(sd(accuracies_1))

print(mean(accuracies_10))
print(sd(accuracies_10))

print(mean(accuracies_100))
print(sd(accuracies_100))
