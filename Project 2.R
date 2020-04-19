# Read in Data to variable data
library(openxlsx)
data<-read.xlsx('DataDownload2015.xlsx', sheet = 3)


# Set up logistic regression

# Subset data to include a response and potential predictors
cols <- c("LILATracts_1And10", "TractLOWI", 
          "TractKids", "TractSeniors",
          "TractWhite", "TractBlack", 
          "TractAsian", "TractNHOPI",
          "TractAIAN", "TractOMultir",
          "TractHispanic", "TractHUNV",
          "Urban","TractSNAP",
          "POP2010", "OHU2010",
          "PovertyRate", "MedianFamilyIncome")

# Split the data into training and test sets
set.seed(111)
sample <- sample.int(nrow(data), floor(0.5*nrow(data)), replace = F)
logreg_train <- data[sample,cols]
logreg_test <- data[-sample, cols]

# Use glm to make a logistic regression model
logreg<-glm(LILATracts_1And10~., data=logreg_train, family="binomial")
summary(logreg)

# Delta G for full model
1 - pchisq(logreg$null.deviance-logreg$deviance,12)

# The predictors TractWhite and OHU2010 did not have
# significance according to Wald test. Running a Likelihood
# Ratio test to see if that subset can be removed

# Subset data
reduced_cols <- c("LILATracts_1And10", "TractLOWI", 
                          "TractKids", "TractSeniors",
                          "TractBlack","TractAsian",
                          "TractNHOPI","TractAIAN",
                          "TractOMultir","TractHispanic",
                          "TractHUNV","Urban",
                          "TractSNAP", "POP2010",
                          "PovertyRate","MedianFamilyIncome")
reduced_data <- logreg_train[,reduced_cols]

# Subset model
reduced <-glm(LILATracts_1And10~., data=reduced_data, family="binomial")

# Likelihood Ratio test
# H0: ^B,TractWhite = ^B, OHU2010 = 0
# H1: At least one of the predictors TractWhite, OHU2010 is not 0

1-pchisq(reduced$deviance-logreg$deviance,2)

# p-value = 0.980
# Fail to reject the null, we can drop OHU and TractWhite
# Renaming reduced to logreg

logreg <- reduced
summary(logreg)

# In the summary for the reduced model, POP2010 is
# now found to be insignificant from its Wald test
# statistic. Removing the POP2010 predictor from the model
reduced_data <- reduced_data[,-14]
logreg <-glm(LILATracts_1And10~., data=reduced_data, family="binomial")
summary(logreg)

# All predictors now have significant Wald Test statistics. 
# Checking Delta G for newest iteration of model
1 - pchisq(logreg$null.deviance-logreg$deviance,12)

#DG = 0

# Create an ROC curve for our Logistic Regression
library(ROCR)

# Predictions for Low Income, Low Access on the test set
# based on fitted model
preds <- predict(logreg,newdata=logreg_test, type="response")

# Classification Rates
rates <- prediction(preds, logreg_test$LILATracts_1And10)

# ROC plot
roc_result <- performance(rates, measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC for Low Income, Low Access Classifier")
lines(x=c(0,1), y=c(0,1),col="red")

auc <- performance(rates, measure="auc")
auc
# AUC of .84 indicates a model better than random guessing
