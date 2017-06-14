# Amber Garner
# Nov. 5, 2016
# Decision Tree on pima diabetes

#*********Setup*************************************** 

# Load credit approval dataset
cont <- read.csv("pima_diabetes.csv", header = T, sep = ",")

#**********DATA PREPROCESSING****

# structure and summary of data
summary(cont)
str(cont)

# Verify changes
summary(cont)

# divide into training (70%) and test (30%) sets
set.seed(1234)
ind <- sample(2, nrow(cont), replace = TRUE, prob = c(0.7, 0.3))
train.data <- cont[ind == 1, ]
test.data <- cont[ind == 2, ]

install.packages("party")
library(party)

# build decision tree with ctree
myFormula <- class~.
model <- ctree(myFormula, data=train.data)

# print credit decision tree model
print(model)

# view nodes starting at the 2nd node
nodes(model, 2)

# plot decision tree model
plot(model)

# plot simple decision tree model
plot(model, type="simple")

#********ACCURACY****************

# build confusion matrix on training data
table(predict(model), train.data$class)

# Calcualte classification accuracy
train_accuracy <-(320+82)/(nrow(train.data))
train_accuracy 

# Calculate classification error rate
train_error <- (99+33)/(nrow(train.data))
train_error

# table of probabilities
prop.table(table(predict(model), train.data$class))

# confusion matrix on test data
testPred <- predict(model, newdata = test.data)
table (testPred, test.data$class)

# Calcualte classification accuracy
test_accuracy <-(133+40)/(nrow(test.data))
test_accuracy 

# Calculate classification error rate
test_error <- (14+47)/(nrow(test.data))
test_error

#*******RANDOM FOREST*******************

install.packages("randomForest")
library(randomForest)

model2 <- randomForest(class~., train.data, ntree=500)
plot(model2)

print(model2)

# Variable Importance Plot
varImpPlot(model)

# build confusion matrix on training data
table(predict(model2), train.data$class)

# Calcualte classification accuracy
train_accuracy <-(300+96)/(nrow(train.data))
train_accuracy 

# Calculate classification error rate
train_error <- (53+85)/(nrow(train.data))
train_error

# table of probabilities
prop.table(table(predict(model2), train.data$class))

# confusion matrix on test data
testPred <- predict(model2, newdata = test.data)
table (testPred, test.data$class)

# Calcualte classification accuracy
test_accuracy <-(131+55)/(nrow(test.data))
test_accuracy 

# Calculate classification error rate
test_error <- (16+32)/(nrow(test.data))
test_error

#*******THE END**************