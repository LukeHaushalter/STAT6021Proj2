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
          "TractHispanic", "TractHUNV","TractSNAP")
logreg_data <- data[,cols]

# Use glm to make a logistic regression model
logreg<-glm(LILATracts_1And10~., data=logreg_data, family="binomial")
summary(logreg)

# Delta G 
1 - pchisq(logreg$null.deviance-logreg$deviance,12)

