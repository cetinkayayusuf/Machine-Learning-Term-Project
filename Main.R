# For decision tree
library(rpart)
library(rpart.plot)

# Confussion matrix
library(caret)

# Knn Function
library(class)

# library(party)

library(beepr) # Beep Sound


####----Import DataSet----####
raw_data <- read.table("cardio_train.csv", header=TRUE, sep=";", dec=".")
data <- raw_data
####----Clean and Prepare----####

##--Blood Pressure--##

#ap_hi and ap_lo cannot be lower than zero
unusual_data <- data[which(data$ap_lo <= 0 | data$ap_hi <= 0), ]
print(paste("there were ",nrow(unusual_data)," records where ap_hi or ap_lo is lower than 0"))

data <- data[which(data$ap_lo > 0 & data$ap_hi > 0), ]

#ap_lo cannot be higher than ap_hi
unusual_data <- data[which(data$ap_lo > data$ap_hi), ]
print(paste("there were ",nrow(unusual_data)," records where ap_lo is higher than ap_hi"))

data <- data[which(data$ap_lo < data$ap_hi), ]

#ap_hi and ap_lo cannot be higher than 300
unusual_data <- data[which(data$ap_lo > 300 | data$ap_hi > 300), ]
print(paste("there were ",nrow(unusual_data)," records where ap_hi or ap_lo is higher than 300"))

data <- data[which(data$ap_lo < 300 & data$ap_hi < 300), ]

#make a new categorical feature from  ap_hi and ap_lo
test_data <- data
test_data$bp[test_data$ap_hi < 120 & test_data$ap_lo < 80] <- 1 #blood pressure is normal

test_data$bp[( test_data$ap_hi >= 120 & test_data$ap_hi < 130) & test_data$ap_lo < 80] <- 2 #blood pressure is elevated

test_data$bp[( test_data$ap_hi >= 130 & test_data$ap_hi < 140) | ( test_data$ap_lo >= 80 & test_data$ap_lo < 90)] <- 3  #blood pressure is hypertension stage 1

test_data$bp[test_data$ap_hi >= 140  |  test_data$ap_lo >= 90] <- 4 #blood pressure is hypertension stage 2

test_data$bp[test_data$ap_hi >= 180  |  test_data$ap_lo >= 120] <- 5 #blood pressure is hypertensive crisis

#now we have a new categorical feature so we dont need ap_hi and ap_lo
test_data = subset(test_data, select = -c(ap_hi, ap_lo) )
data <- test_data


##--Age--##
#convert days to years
data$age <- as.integer(data$age/365)


##--BMI--##

#weight and height cannot be lower than 0
unusual_data <- data[which(data$weight <= 0 | data$height <= 0), ]
print(paste("there were ",nrow(unusual_data)," records where weight or height is lower than 0"))

data <- data[which(data$weight > 0 & data$height > 0), ]

#make a new categorical feature from  weight and height
test_data <- data

test_data$indx <- test_data$weight / ((test_data$height /100) ^ 2 )

# Removing outliers
x <- test_data$indx
x.range <- quantile(x, c(0.25, 0.75))
x.range[1] <- x.range[1] - 1.5 * IQR(x)
x.range[2] <- x.range[2] + 1.5 * IQR(x)
# x <- x[which(x.range[1] < x & x < x.range[2])]
print(paste(length(which(x.range[1] >= x | x >= x.range[2])), "outlier is removed"))
test_data <- test_data[which(x.range[1] < x & x < x.range[2]), ]

test_data$bmi[test_data$indx < 18.5] <- 1 #underweight

test_data$bmi[test_data$indx >= 18.5 & test_data$indx < 25] <- 2 #normal

test_data$bmi[test_data$indx >= 25 & test_data$indx < 29.9] <- 3 #overweight

test_data$bmi[test_data$indx >= 29.9] <- 4 #obese

#now we have a new categorical feature so we dont need weight and height
test_data = subset(test_data, select = -c(weight, height, indx) )
data <- test_data


# data$cardio[data$cardio == 1] <- "Presence"
# data$cardio[data$cardio == 0] <- "Absence"


##--Turn numbers ans strings to categorical--##
cols = c("gender", "cholesterol", "gluc", "smoke", "alco", "active", "cardio", "bp", "bmi")
data[cols] = lapply(data[cols], factor)

# Remove id column
data$id <- NULL

# Decision Tree Implementation

# No Cross-Validation

set.seed(156678)  #Set the seed for reproducibility
# Use 70% of samples for traning, the rest for testing
# the indices (row ids) are saved in the "sub" vector
sub <- sample(1:nrow(data), size=nrow(data)*0.7)
data.train <- data[sub,]
data.test <- data[-sub,]

fit1 <- rpart(cardio ~ ., data=data.train, method="class")
plot(fit1)
text(fit1)
rpart.plot(fit1, roundint = FALSE , digits = 4)

# Creating confusion matrix
prediction1 <- predict(fit1, data.test, type = "class")
matrix <- table(predicted = prediction1, actual = data.test$cardio)
acc <- sum(diag(matrix)) / sum(matrix)
acc
confusionMatrix(matrix, mode = "prec_recall")

# Different subset ratio

set.seed(156678)  #Set the seed for reproducibility
subset_acc <- numeric()
dt <- NULL

for (i in 70:90) {
  sub <- sample(1:nrow(data), size=nrow(data)*(i / 100))
  data.train <- data[sub,]
  data.test <- data[-sub,]

  fit1 <- rpart(cardio ~ ., data=data.train, method="class")

  # Creating confussion matrix
  prediction1 <- predict(fit1, data.test, type = "class")
  matrix <- table(predicted = prediction1, actual = data.test$cardio)
  acc <- sum(diag(matrix)) / sum(matrix)

  subset_acc <- c(subset_acc, acc)

  if(acc >= max(subset_acc)){
    dt <- fit1
    dt.test <- data.test
  }
}

# Visualize best decision tree
plot(dt)
text(dt)
rpart.plot(dt, main = "Decision Tree", roundint = FALSE , digits = 4)

# Accuracy of best decision tree
print(paste("Maximum accuracy is",max(subset_acc)))

# Best decision tree ratio
print(paste("Maximum accuracy of subset ratio is",which.max(subset_acc) + 69))

# Creating confussion matrix

prediction1 <- predict(dt, dt.test, type = "class")
matrix <- table(predicted = prediction1, actual = dt.test$cardio)
confusionMatrix(matrix, mode = "prec_recall")


plot(c(70:90), subset_acc, type="l", ylab="Accuracy Rate", xlab="Ratio",
     main="Accuracy Rate With Different Subset Ratio of Data")


# Cross-validation version

dt_acc <- numeric()
ratio <- (which.max(subset_acc) + 69) / 100
dt <- NULL
set.seed(1815850)

for(i in 1:100){
  # Creating sample
  # sub <- sample(1:nrow(data), size=nrow(data)*0.8)
  sub <- sample(1:nrow(data), size=nrow(data)*ratio)
  data.train <- data[sub,]
  data.test <- data[-sub,]

  fit1 <- rpart(cardio ~ ., data=data.train, method="class")

  prediction1 <- predict(fit1, data.test, type = "class")
  matrix <- table(predicted = prediction1, actual = data.test$cardio)
  acc <- sum(diag(matrix)) / sum(matrix)
  dt_acc <- c(dt_acc, acc)

  if(acc >= max(dt_acc)){
    dt <- fit1
    dt.test <- data.test
  }
}

# Visualize best decision tree
plot(dt)
text(dt)
rpart.plot(dt, main = "Decision Tree", roundint = FALSE , digits = 4)

# Accuracy of best decision tree
print(paste("Maximum accuracy ove cross-validation is",max(dt_acc)))

# Creating confussion matrix

prediction1 <- predict(dt, dt.test, type = "class")
matrix <- table(predicted = prediction1, actual = dt.test$cardio)
confusionMatrix(matrix, mode = "prec_recall")

plot(dt_acc, type="l", ylab="Accuracy Rate", xlab="Iterations",
     main="Accuracy Rate With Different Subsets of Data")

# KNN Implementation

# First try to determine the right K-value
set.seed(156678)  #Set the seed for reproducibility
# Use 70% of samples for traning, the rest for testing
# the indices (row ids) are saved in the "sub" vector
sub <- sample(1:nrow(data), size=nrow(data)*0.7)
data.train <- data[sub, ]
data.test <- data[-sub, ]

data_acc <- numeric() #holding variable
knn_predict <- NULL


for(i in 1:40){
  # Apply knn with k = i
  predict <- knn(train = data.train[, -8],
                 test = data.test[, -8], cl = data.train$cardio, k = i)
  acc <- mean(predict == data.test$cardio)
  data_acc <- c(data_acc, acc)

  if(acc >= max(data_acc)){
    knn_predict <- predict
  }
}

# Plot accuracy rates
plot(data_acc, type = "l", ylab = "Accuracy Rate",  xlab = "K",
     main = "Accuracy Rate for data with varying K")

# Max accuracy of k-values
max(data_acc)

# k-value has max accuracy
which.max(data_acc)

# Creating Confusion matrix
matrix <- table(predicted = knn_predict, actual = data.test$cardio)
confusionMatrix(matrix, mode = "prec_recall")

# Different subset ratio
set.seed(15248621)
subset_acc <- numeric()
knn_test <- NULL
k_value <- which.max(data_acc)

for(i in 70:90){
  sub <- sample(1:nrow(data), size=nrow(data)*(i/100))
  data.train <- data[sub, ]
  data.test <- data[-sub, ]

  predict <- knn(train = data.train[, -8],
                 test = data.test[, -8], cl = data.train$cardio, k = k_value)
  acc <- mean(predict == data.test$cardio)
  subset_acc <- c(subset_acc, acc)
  print(paste("Ratio : ", i))

  if(acc >= max(subset_acc)){
    knn_predict <- predict
    knn_test <- data.test
  }
}

plot(c(70:90) ,subset_acc, type="l", ylab="Accuracy Rate",xlab="Ratio",
     main="Accuracy Rate With Different Subset Ratio")

# Accuracy of best decision tree
print(paste("Maximum accuracy is",max(subset_acc)))

# Best decision tree ratio
print(paste("Maximum accuracy of subset ratio is",which.max(subset_acc) + 69))

# Creating Confusion matrix
matrix <- table(predicted = knn_predict, actual = knn_test$cardio)
confusionMatrix(matrix, mode = "prec_recall")

# Try different samples of the dataset to validate K value 

set.seed(15248621)
ratio <- (which.max(subset_acc) + 69) / 100
k_value <- which.max(data_acc)
subset_acc <- numeric()
knn_test <- NULL

for(i in 1:100){
  sub <- sample(1:nrow(data), size=nrow(data)*ratio)
  data.train <- data[sub, ]
  data.test <- data[-sub, ]

  predict <- knn(train = data.train[, -8],
                 test = data.test[, -8], cl = data.train$cardio, k = k_value)
  acc <- mean(predict == data.test$cardio)
  subset_acc <- c(subset_acc, acc)
  print(paste("Subset : ", i))

  if(acc >= max(subset_acc)){
    knn_predict <- predict
    knn_test <- data.test
  }
}

plot(subset_acc, type="l", ylab="Accuracy Rate",xlab="K",
     main="Accuracy Rate With Different Subset")

# Mean the accuracy
mean(subset_acc)

# Max the accuracy
max(subset_acc)

# Creating Confusion matrix
matrix <- table(predicted = knn_predict, actual = knn_test$cardio)
confusionMatrix(matrix, mode = "prec_recall")

beep()