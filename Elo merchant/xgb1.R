# xgb ---------------------------------------------------------------------
require(xgboost)
train <- read.csv('mytrain.csv')
test <- read.csv('mytest.csv')

train <- as.matrix(train)
test <- as.matrix(test)
dtrain <- xgb.DMatrix(data = train[,c(1:152)], label = train[,154])
bstDMatrix <- xgboost(data = dtrain, max.depth = 4, eta = 0.6, nthread = 4, nrounds = 400, objective = "binary:logistic")

pred <- predict(bstDMatrix, test[,c(1:152)])
prediction <- as.numeric(pred > 0.5)
sum(prediction == 1)
pos <- which(prediction == 1)
write.csv(pos, file = "prediction.csv")
pos

# RF ----------------------------------------------------------------------
require(randomForest)
rf <- randomForest(train[,c(1:152)], as.factor(train[,154]), ntree=100, importance=TRUE)
pred2 <- predict( rf, test )
prediction <- as.numeric(pred2 > 0.5)
sum(pred2 == 1)
pos2 <- which(pred2 == 1)
write.csv(pos2, file = "prediction2.csv")
pos2

