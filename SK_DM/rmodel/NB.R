# Import libraries
library(e1071)
library(rminer)

# Create a dataframe from Final Data:
df <- read.csv("C:/SK/SK_DM/data/cs-training-cf-h.csv")

# Partition the dataset into training and testing sets:
library(caret)
set.seed(500)
indTrain <- createDataPartition(df$Dlnq, p=0.7, list=FALSE)
trainDF <- df[indTrain,]
testDF <- df[-indTrain,]

# Build the Model:
nbm <- naiveBayes(as.factor(Dlnq)~., data=trainDF)
nbm

# Make prediction using this Model:
nbpredict <- predict(nbm, testDF)

# Analyze prediction and print Confusion Matrix:
head(nbpredict, n=5)
table (testDF$Dlnq, nbpredict)