# Import libraries
library(rpart)
library(rpart.plot)

# Create a dataframe from Final Data:
df <- read.csv("C:/SK/SK_DM/data/cs-training-cf-h.csv")

# Partition the dataset into training and testing sets:
library(caret)
set.seed(500)
indTrain <- createDataPartition(df$Dlnq, p=0.7, list=FALSE)
trainDF <- df[indTrain,]
testDF <- df[-indTrain,]

# Build the Model:
dtm <- rpart(Dlnq~., data=trainDF, method="class")
dtm
rpart.plot(dtm, type=4, extra=101)

# Make prediction using this Model:
dtpredict <- predict(dtm, testDF, type="class")

# Analyze prediction and print Confusion Matrix:
table (testDF$Dlnq, dtpredict)