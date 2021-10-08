install.packages("caret")
library(caret)
data(iris)
dataset <- iris
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]
dim(dataset)
sapply(dataset, class)
head(dataset)
levels(dataset$Species)
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)
#  Summary of attribute distributions, self explanatory
summary(dataset)

#  splitting input and output for plotting
x <- dataset[,1:4]
y <- dataset[,5]

#  doing some boxplottin!
dev.new(width=10, height=10)
par(mfrow=c(1,4))
  for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
  }
#  class breakdown
plot(y)

#scatterplot time!
featurePlot(x=x, y=y, plot="box")

#  using density plots of each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#  10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#  linear algorithm
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
#  nonlinear algorithms: CART and kNN
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
#  advanced algorithms: SVM and Random Forest
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

#  getting the accuracy of the models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

#visualizing the results
dotplot(results)

#lda is the best of the models
print(fit.lda)

#time to makes some predictions out of the model
#estimating the skill of LDA on the valication dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)

#Model is proven to predict the separated 20% of the dataset within +/- 4% of 97% accuracy