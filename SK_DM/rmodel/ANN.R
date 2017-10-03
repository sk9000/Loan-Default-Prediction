# Import libraries:
installed.packages("neuralnet")
require(neuralnet)

# Create a dataframe from Final Data:
df <- read.csv("C:/SK/SK_DM/data/cs-training-cf-h.csv")

# Partition the dataset into training and testing sets:
library(caret)
set.seed(500)
indTrain <- createDataPartition(df$Dlnq, p=0.7, list=FALSE)
trainDF <- df[indTrain,]
testDF <- df[-indTrain,]


# Build the Model with Dlnq as Integer:
nnm1 = neuralnet(Dlnq~RevUtil+Age+DebtRatio+MonthlyInc+NumOpenCredit+NumREloans+NumDlnq_30_59+
                   NumDlnq_60_89+NumDlnq_90_Plus+NumDep, data=trainDF, hidden=1, linear.output = FALSE)

# Plot the model graphically:
plot(nnm1)

# View Results and Weight Matrix:
nnm1$result.matrix
nnm1$weights

# Make prediction using this Model:
nnm1predict <- compute(nnm1,testDF[,2:11], rep=1)

# Analyze prediction and print Confusion Matrix:
table(testDF$Dlnq)
table(nnm1predict$net.result)
#-------decide cut-off based on net.result values-------------
table(testDF$Dlnq, nnm1predict$net.result > 0.0696546761026944)

