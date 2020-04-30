#STATS 6021 - Project 2
#Quantitative Modeling to Predict SNAP Benefits in a Census Tract
install.packages("openxlsx")
install.packages("olsrr")
library(openxlsx)
library(faraway)
library(MASS)
library(olsrr)

data <- read.xlsx("DataDownload2015.xlsx", sheet = 3)

#reduce the unnecessary columns of data
cols <- c("PCTGQTRS","TractLOWI", 
          "TractKids", "TractSeniors",
          "TractWhite", "TractBlack", 
          "TractAsian", "TractNHOPI",
          "TractAIAN", "TractOMultir",
          "TractHispanic", "TractHUNV",
          "Urban","TractSNAP",
          "POP2010", "OHU2010",
          "PovertyRate", "MedianFamilyIncome")

data <- data[,cols]

####### Sampled data set for easier calculations #######

set.seed(200)
sample<-sample.int(nrow(data), floor(.01*nrow(data)), replace = F) #1% of the data is still 700+ observations
train<-data[sample, ]
test<-data[-sample, ]

attach(train)
train

result_train <- lm(TractSNAP~., data=train)

summary(result_train)

round(vif(result_train),2)
#VIFS: SIGNIFICANT MULTICOLLINEARITY IN THE DATA
#  OHU2010 - 9,060
#  MedianFamilyIncome - 6,733
#  POP2010 - 75

#APPROACH 1: REMOVE CERTAIN REGRESSORS
cols_reduced <- c("TractLOWI", 
          "TractKids", "TractSeniors",
          "TractWhite", "TractBlack", 
          "TractAsian", "TractNHOPI",
          "TractAIAN", "TractOMultir",
          "TractHispanic", "TractHUNV",
          "Urban","TractSNAP",
          "PovertyRate")

train_reduced <- train[,cols_reduced]

result_train_reduced <- lm(TractSNAP~., data=train_reduced)
summary(result_train_reduced)

round(vif(result_train_reduced),2)

plot(result_train_reduced$fitted.values,result_train_reduced$residuals)
abline(h=0)
plot(result_train$fitted.values,result_train$residuals)

######### RESET VALUES TO PROPORTIONS ###########
#For ease of interpretation and scaling to different tracts
train$Snap_pct <- train$TractSNAP/train$OHU2010*100 #proportion of housing units receiving snap benefits
train$White_pct <- train$TractWhite/train$POP2010*100
train$Black_pct <- train$TractBlack/train$POP2010*100
train$Asian_pct <- train$TractAsian/train$POP2010*100
train$AIAN_pct <- train$TractAIAN/train$POP2010*100
train$NHOPI_pct <- train$TractNHOPI/train$POP2010*100
train$OMultir_pct <- train$TractOMultir/train$POP2010*100
train$LOWI_pct <- train$TractLOWI/train$POP2010*100 
train$GroupQtrs_pct <- train$PCTGQTRS*100

######### IMPORTANT TRANSFORMATION ###########
# Median income needed an 1/x transformation as related to snap benefits
par(mfrow=c(1,1))
plot(train$MedianFamilyIncome, train$Snap_pct)

#Transformation
train$MedianIncome_transf <- 1/train$MedianFamilyIncome
par(mfrow=c(1,2))
plot(train$MedianIncome_transf, train$Snap_pct, main = "Transformed")
plot(train$MedianFamilyIncome, train$Snap_pct, main = "Without Tranformation")

##############################################


######### REMOVE INF, -INF VALUES ###########
#df <- df[is.finite(rowSums(df)),]
#https://stackoverflow.com/questions/36590230/how-to-remove-rows-with-inf-from-a-dataframe-in-r

#removes infinite/NA rows from training set
train_adj <- train[is.finite(rowSums(train)),]
train_adj

####### ITERATING THROUGH THE TRACT % DEMOGRAPHICS #######
#note: First time I only used racial demographics
#note: Second time I included % low income
#note: Third time I included GroupQtrs (no change)

cols_reduced_2 <- c("Snap_pct", 
                  "White_pct", "Black_pct", 
                  "Asian_pct", "NHOPI_pct",
                  "AIAN_pct", "OMultir_pct", "LOWI_pct", "GroupQtrs_pct",
                  "MedianIncome_transf")

train_adj_reduced <- train_adj[,cols_reduced_2]

new_data <- train_adj_reduced

regnull_new <- lm(Snap_pct~1, data=new_data)
regfull_new <- lm(Snap_pct~., data=new_data)

step(regnull_new, scope=list(lower=regnull_new, upper=regfull_new), direction="forward") #Median + LOWI + Black_pct
step(regfull_new, scope=list(lower=regnull_new, upper=regfull_new), direction="backward") #same as forward
step(regnull_new, scope=list(lower=regnull_new, upper=regfull_new), direction="both") #same as forward

#Graphical comparison of the individual values against Snap %
par(mfrow=c(1,3))
plot(train$MedianIncome_transf, train$Snap_pct, main = "Median Income")
plot(train$LOWI_pct, train$Snap_pct, main = "LOWI %")
plot(train$Black_pct, train$Snap_pct, main = "Black %") #this one looks the most off

result_forward <- lm(Snap_pct~MedianIncome_transf+Black_pct+LOWI_pct, data = train_adj_reduced)
summary(result_forward)
#adjR of 0.7582

plot(result_forward$fitted.values, result_forward$residuals)
abline(h=0)

acf(result_forward$residuals)
qqnorm(result_forward$residuals)
qqline(result_forward$residuals)


##############################################
############# OUTLIER ANALYSIS ###############
##############################################

###### Outlier/Leverage Charts #########
ols_plot_resid_stud(result_forward) #60,490,595
ols_plot_cooksd_chart(result_forward) #490,251,595
ols_plot_resid_lev(result_forward)


###### REMOVE HIGH LEVERAGE/OUTLIER POINTS #########
#remove top 3
train_lev <- train_adj[-c(490,251,595),]


####### REASSESS REGRESSION MODEL ##########
result_lev <- lm(Snap_pct~MedianIncome_transf+LOWI_pct+Black_pct, data = train_lev)
summary(result_lev)
#adjR of 0.7786

vif(result_lev) #pass

plot(result_lev$fitted.values, result_lev$residuals)
abline(h=0)

acf(result_lev$residuals)
qqnorm(result_lev$residuals)
qqline(result_lev$residuals)







