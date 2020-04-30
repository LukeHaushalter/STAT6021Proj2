# Read in Data to variable data
library(openxlsx)
data<-read.xlsx('DataDownload2015.xlsx', sheet = 3)

# What % of tracts are urban?
num.urban <- sum(data$Urban == 1)
num.urban/nrow(data)
# 75.72%

# US Population accounted for in data
us.pop <- sum(data$POP2010)
# Approx. 308 mil

# Population by state
state.pops <- aggregate(data$POP2010,by=list(State=data$State), FUN=sum )

# Racial populations by state
states.white.pop <- aggregate(data$TractWhite, list(State=data$State), FUN=sum)
states.black.pop <- aggregate(data$TractBlack, list(State=data$State), FUN=sum)
states.asian.pop <- aggregate(data$TractAsian, list(State=data$State), FUN=sum)
states.nhopi.pop <- aggregate(data$TractNHOPI, list(State=data$State), FUN=sum)
states.aian.pop <- aggregate(data$TractAIAN, list(State=data$State), FUN=sum)
states.omultir.pop <- aggregate(data$TractOMultir, list(State=data$State), FUN=sum)

# Relative frequencies of racial populations by state
states.white.pop$x <- states.white.pop$x/state.pops$x
states.black.pop$x <- states.black.pop$x/state.pops$x
states.asian.pop$x <- states.asian.pop$x/state.pops$x
states.nhopi.pop$x <- states.nhopi.pop$x/state.pops$x
states.aian.pop$x <- states.aian.pop$x/state.pops$x
states.omultir.pop$x<-states.omultir.pop$x/state.pops$x

# Can we infer anything about food access by looking a a tract's 
# racial demographic makeup?

# Add columns for relative freq of races
data$pctWhite <- data$TractWhite/data$POP2010
data$pctBlack <- data$TractBlack/data$POP2010
data$pctAsian <- data$TractAsian/data$POP2010
data$pctNHOPI <- data$TractNHOPI/data$POP2010
data$pctAIAN <- data$TractAIAN/data$POP2010
data$pctOMultir <- data$TractOMultir/data$POP2010

# Create a dataframe to plot
df <- rbind(
  data.frame("State" = states.nhopi.pop$State, "pct"=states.nhopi.pop$x, "Race"="Native Hawaiian/Pacific Islander"),
  data.frame("State" = states.aian.pop$State, "pct"=states.aian.pop$x, "Race"="American Indian/Native Alaskan"),
  data.frame("State" = states.omultir.pop$State, "pct"=states.omultir.pop$x, "Race"="Multi-Racial/Other"),
  data.frame("State" = states.asian.pop$State, "pct"=states.asian.pop$x, "Race"="Asian"),
  data.frame("State" = states.black.pop$State, "pct"=states.black.pop$x, "Race"="Black"),
  data.frame("State" = states.white.pop$State, "pct"=states.white.pop$x, "Race"="White")
)

library(ggplot2)
ggplot(df, aes(x=State, y=pct)) + 
  geom_bar(aes(fill=Race), stat="identity")

# Plots by race

state.low.access.pop <- aggregate(data$lapop1, list(State=data$State), FUN=sum)
state.low.access.pop[2] <- state.low.access.pop[2]/state.pops[2]


white.pop.la.pop <- cbind(states.white.pop, state.low.access.pop[2])
colnames(white.pop.la.pop)<- c("State", "Rel. White Pop.", "Rel. LA Pop.")
plot(white.pop.la.pop$`Rel. White Pop.`, white.pop.la.pop$`Rel. LA Pop.`)

black.pop.la.pop <- cbind(states.black.pop, state.low.access.pop[2])
colnames(black.pop.la.pop)<- c("State", "Rel. Black Pop.", "Rel. LA Pop.")
plot(black.pop.la.pop$`Rel. Black Pop.`, black.pop.la.pop$`Rel. LA Pop.`)

asian.pop.la.pop <- cbind(states.asian.pop, state.low.access.pop[2])
colnames(asian.pop.la.pop)<- c("State", "Rel. Asian Pop.", "Rel. LA Pop.")
plot(asian.pop.la.pop$`Rel. Asian Pop.`, asian.pop.la.pop$`Rel. LA Pop.`)

nhopi.pop.la.pop <- cbind(states.nhopi.pop, state.low.access.pop[2])
colnames(nhopi.pop.la.pop)<- c("State", "Rel. NHOPI Pop.", "Rel. LA Pop.")
plot(nhopi.pop.la.pop$`Rel. NHOPI Pop.`, nhopi.pop.la.pop$`Rel. LA Pop.`)

aian.pop.la.pop <- cbind(states.aian.pop, state.low.access.pop[2])
colnames(aian.pop.la.pop)<- c("State", "Rel. AIAN Pop.", "Rel. LA Pop.")
plot(aian.pop.la.pop$`Rel. AIAN Pop.`, aian.pop.la.pop$`Rel. LA Pop.`)

omultir.pop.la.pop <- cbind(states.omultir.pop, state.low.access.pop[2])
colnames(omultir.pop.la.pop)<- c("State", "Rel. Multi-racial/Other Pop.", "Rel. LA Pop.")
plot(omultir.pop.la.pop$`Rel. Multi-racial/Other Pop.`, omultir.pop.la.pop$`Rel. LA Pop.`)


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


confint(logreg)

# Could some variables be dropped
# Analysis of Deviance - Deviances by variable
deviance.by.variable <- anova(logreg)$Deviance

# Plotted and ordered - Is there an "elbow?"
plot(deviance.by.variable[order(deviance.by.variable, decreasing = T)])

