# Needed libraries

> install.packages("caret", dependencies = TRUE)
> library(caret)
> library(randomForest)
> install.packages("fields")
> library(fields)

# Download the data and take a look

> trainData <- read.table("train.csv", sep = ",", header = TRUE)
> testData <- read.table("test.csv", sep = ",", header = TRUE)
> head(trainData)
> head(testData)

# Well, the difference is just the missing label for whether a person survived

# We can check if a particular variable is useful for the prediction

> bplot.xy(trainData$Survived, trainData$Pclass)
> bplot.xy(trainData$Survived, trainData$Age)


# We need to convert the label in order to train the algorithm
> trainData$Survived <- factor(trainData$Survived)

# Train with random forest, Age seems useless so we don’t use it


> model <- train(Survived ~ Pclass + Sex + SibSp + Embarked + Parch + Fare,
+                data = trainData,
+                method = "rf",
+                trControl = trainControl(method = "cv",
+                                         number = 5)
+ )


# Now lets take a look a the test set again

> head(testData)

# Here we need to remove the N/As

> testData <- ifelse(is.na(testData$Fare), mean(testData$Fare, na.rm = TRUE), testData$Fare)

# Now we just do the prediction and save it

> testData$Survived <- predict(model, newdata = testData)
> prediction <- testData[,c("PassengerId", "Survived")]
> write.table(prediction, file="prediction.csv", col.names = TRUE, row.names = FALSE, sep = ",")

#Score of .77 on Kaggle, a deeper analysis of the variables should lead to better score.
