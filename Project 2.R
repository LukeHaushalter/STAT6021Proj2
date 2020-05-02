
library(openxlsx)
library(caret)
library(pscl)
# Read in Data to variable data

data<-read.xlsx('DataDownload2015.xlsx', sheet = 3)
View(data)

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
logreg_data <- data[,cols]

# Convert assumed categorical variables to factors (LILATracts_1And10, Urban)
logreg_data$LILATracts_1And10 <- factor(logreg_data$LILATracts_1And10)
is.factor(logreg_data$LILATracts_1And10)

logreg_data$Urban <- factor(logreg_data$Urban)
is.factor(logreg_data$Urban)


#Splitting the data into training and test sets

Train <- createDataPartition(logreg_data$LILATracts_1And10, p=0.6, list=FALSE)
training <- logreg_data[ Train, ]
testing <- logreg_data[ -Train, ]

#Fit full logisitic regression model on training data set
logreg_full <- glm(LILATracts_1And10~., data=training, family="binomial")
summary(logreg_full)

# Delta G for full model using training data
1 - pchisq(logreg_full$null.deviance-logreg_full$deviance,16)


#Drop POP2010, OHU2010, and TractWhite from model
logreg_reduced <- glm(LILATracts_1And10~TractLOWI+TractKids+TractSeniors+TractBlack+TractAsian+TractNHOPI+TractAIAN+
                         TractOMultir+TractHispanic+TractHUNV+Urban+TractSNAP+PovertyRate+MedianFamilyIncome, data=training, family="binomial")
summary(logreg_reduced)


# Delta G for reduced model using training data, Ho: POP2010 = OHU2010 = TractWhite = 0, Outcome: Fail to reject Null
1 - pchisq(logreg_reduced$deviance-logreg_full$deviance,3)


#Likelihood Ratio Test: Ho: POP2010 = OHU2010 = TractWhite = 0, Outcome: Fail to reject Null
anova(logreg_full, logreg_reduced, test ="Chisq")

#McFadden's R2 Test: McFadden ~ 0.23 (weak predictive power)
pR2(logreg_reduced)

#Variable Importance Check
varImp(logreg_reduced)

#Wald Test: 
regTermTest(logreg_reduced, "TractOMultir")

regTermTest(logreg_reduced, "TractBlack")

#Drop TractBlack and TractOMultir from model

logreg_reduced_2 <- glm(LILATracts_1And10~TractLOWI+TractKids+TractSeniors+TractAsian+TractNHOPI+TractAIAN+
                        TractHispanic+TractHUNV+Urban+TractSNAP+PovertyRate+MedianFamilyIncome, data=training, family="binomial")
summary(logreg_reduced_2)

# Delta G for reduced model using training data, Ho: TractBlack = TractOMultir = 0, Outcome: Reject Null
1 - pchisq(logreg_reduced_2$deviance-logreg_reduced$deviance, 2)

#Validation of predicted values
pred <- predict(logreg_reduced, newdata=testing, type="response")
accuracy <- table(pred, testing[,"LILATracts_1And10"])
sum(diag(accuracy))/sum(accuracy)
