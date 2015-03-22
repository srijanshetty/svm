# Read data
setwd(".")
spambase <- read.csv("./spambase.data", header=FALSE)

# Import libraries
library(e1071)
library(plyr)

# As all the spam data is at the top, we shuffle
spam <- spambase[sample(nrow(spambase)), ]

# k-fold validation
k = 5
n = floor(nrow(spam)/k)
err.vec = vector()

for (fold in 1:k) {
	s1 = ((fold - 1) * n)
	s2 = (fold * n)
	subset = s1:s2

	# Create training and test sets
	data.test = spam[subset, ]
	data.train = spam[-subset, ]

	# Create a SVM model
	model = svm(x = data.train[, -58], y = as.factor(data.train[, 58]))

	# Predict for the test set using the generated model
	prediction = predict(model, newdata = data.test[, -58])

	# Compute the error
	err.vec[fold] = count(data.test$V58 != prediction)[2,2] / nrow(data.test)
}

# Compute the error
error = mean(err.vec)
