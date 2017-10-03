# Create a dataframe from Final Data:
df <- read.csv("C:/SK/SK_DM/data/cs-training-cf-h.csv")

# Partition the dataset into training and testing sets:
library(caret)
set.seed(500)
indTrain <- createDataPartition(df$Dlnq, p=0.7, list=FALSE)
trainDF <- df[indTrain,]
testDF <- df[-indTrain,]

# Build the Model with backwar and forward directions separately:
lrb <- step(lm (formula = Dlnq~RevUtil+Age+DebtRatio+MonthlyInc+NumOpenCredit+NumREloans+
                  NumDlnq_30_59+NumDlnq_60_89+NumDlnq_90_Plus+NumDep, data = trainDF), direction = "backward")
#---------------------------------
lrf <- step(lm (formula = Dlnq~1, data = trainDF), direction = "forward", scope =~RevUtil+Age+
              DebtRatio+MonthlyInc+NumOpenCredit+NumREloans+NumDlnq_30_59+NumDlnq_60_89+NumDlnq_90_Plus+NumDep)

# Note: Both backward and forward options eliminated same variables: RevUtil & NumOpenCredit
# Build the GLM Model with remaining 8 variables:
f = Dlnq~ Age+DebtRatio+MonthlyInc+ NumREloans+NumDlnq_30_59+NumDlnq_60_89+NumDlnq_90_Plus+NumDep
lrm <- glm (formula = f, data = trainDF, family = "binomial")
#----------------------------
lrm
#----------------------------
summary(lrm)

# Make prediction using this Model:
lrpredict <- ifelse(predict(lrm, data=testDF, type="response")>0.5, TRUE, FALSE)

# Analyze prediction and print Confusion Matrix:
table(lrpredict, as.logical(lrm$y))

