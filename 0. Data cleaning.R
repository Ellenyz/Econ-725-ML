
rm(list=ls())
# Loading libraries required and reading the data into R
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(tidyverse)
library(haven)
library(haven)
library(lubridate)
library(ggplot2)
library(cowplot)
library(randomForest)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(rsample)  # data splitting 
library(glmnet)   # implementing regularized regression approaches
library(dplyr)    # basic data manipulation procedures
library(ggplot2)  # plotting
library(pdp)

# Load Data
setwd("/Users/sachin/Downloads/Econ_725/Projects/code")
df1 <- read_csv("cleaned_data.csv")


# Dim
dim(df1)

# #display first 10 variables and the response variable
str(df1[,c(1:10, 81)]) 

# The response variable; biddy1
ggplot(data=df1[!is.na(df1$biddy1),], aes(x=biddy1)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

# The response variable; logbid1
ggplot(data=df1[!is.na(df1$logbid1),], aes(x=logbid1)) +
  geom_histogram(fill="blue", binwidth = 1) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

# summary
summary(df1$logbid1)

#
df1 <- df1[!(is.na(df1$logbid1)),]
df1 <- df1[!(is.na(df1$photos)),]
df1 <- df1[!(is.na(df1$logmiles)),]
df1 <- df1[!(is.na(df1$logtext)),]

# find most na columns
colSums(is.na(df1))

# delete Na columns
#delete columns
cok=apply(df1,2,function(x)!any(is.na(x)))
df1 = df1[,cok] 

#
#ggplot(data=df1[!is.na(df1$logbid1),], aes(x=factor(sell), y=logbid1))+
#  geom_boxplot(col='blue') + labs(x='Overall Quality') +
#  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)


###################################################################
# For logbid1

#data_ebay <- subset(df1, select = c("logbid1", "photos", "logmiles", "logtext", "ding"))

# Correlations with logbid1
numericVars <- which(sapply(df1, is.numeric)) #index vector numeric variables
# numericVarNames <- names(numericVars) #saving names vector for use later on
# cat('There are', length(numericVars), 'numeric variables')

#
data_ebay <- df1[, numericVars]
#cor_numVar <- cor(data_ebay, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with logbid1
#cor_sorted <- as.matrix(sort(cor_numVar[,'logbid1'], decreasing = TRUE))
#select only high corelations
#CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))
#cor_numVar <- cor_numVar[CorHigh, CorHigh]

#corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

# Only sell car
data_ebay <- data_ebay[data_ebay$sell ==1, ]

# order data
col_order <- c(383, 1:382, 384:457)
data_ebay <- data_ebay[, col_order]

# delete biddy1
data_ebay = subset(data_ebay, select = -c(biddy1) )

# Summary Stat data
data_sum = subset(data_ebay, select = c(logbid1, sell, miles, startbid, warranty, cyl, model, photos, doors,
                                         rust_group, logtext, reserve, buyitnow, warranty, numbids) )

# save data summary in latex
library(stargazer)
stargazer(as.data.frame(data_sum), type = "latex", digits=1,flip = FALSE, out = 'tab.txt')

####################################################################
# Ridge
######################################
set.seed(0)
smp_size = floor(0.98*nrow(data_ebay))
train_ind <- sample(seq_len(nrow(data_ebay)), size = smp_size)

traindata = data_ebay[train_ind, ]
testdata = data_ebay[-train_ind, ]

data_train_x <- model.matrix(logbid1 ~ ., traindata)[, -1]
data_train_y <- traindata$logbid1

data_test_x <- model.matrix(logbid1 ~ ., testdata)[, -1]
data_test_y <- testdata$logbid1

dim(data_train_x)

##########################################################################
# Ridge
# some best model
cv_ridge   <- cv.glmnet(data_train_x, data_train_y, alpha = 0)
min(cv_ridge$cvm)

# predict
pred_r <- predict(cv_ridge, s = cv_ridge$lambda.min, data_test_x)
# Over all mse
mse_ridge <- mean((data_test_y - pred_r)^2)
mse_ridge

# RMSE
rmse_ridge = sqrt(mse_ridge)


# Variable Importance
varImp <- function(object, lambda = NULL, ...) {
  
  ## skipping a few lines
  
  beta <- predict(object, s = lambda, type = "coef")
  if(is.list(beta)) {
    out <- do.call("cbind", lapply(beta, function(x) x[,1]))
    out <- as.data.frame(out, stringsAsFactors = TRUE)
  } else out <- data.frame(Overall = beta[,1])
  out <- abs(out[rownames(out) != "(Intercept)",,drop = FALSE])
  out
}

var_imp = varImp(cv_ridge, lambda = cv_ridge$lambda.min)
var_imp = as.data.frame(var_imp)
var_imp$names <- rownames(var_imp)
#write_csv(var_imp, "var_imp.csv")

# 
ridgeImportance <- var_imp[order(var_imp$Overall, decreasing = TRUE),]

#
varsSelected <- length(which(ridgeImportance$Overall!=0))
varsNotSelected <- length(which(ridgeImportance$Overall==0))

cat('Ridge uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')

#### Top 25 influential variables in ridge
coef(cv_ridge, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

##

######################################################################
# Random Forest Model
suppressMessages(library(h2o))
suppressMessages(library(randomForest))
suppressMessages(library(neuralnet))
suppressMessages(library(data.table))
suppressMessages(library(stargazer))
suppressMessages(library(glmnet))
suppressMessages(library(knitr))
suppressMessages(library(tidyverse))
set.seed(0)
h2o.init()

rf <- h2o.randomForest(x=2:457, y = 1, training_frame = as.h2o(traindata),
                       validation_frame = as.h2o(testdata), ntree = 50, seed = 420) 

# mse
mse_rf <- h2o.mse(rf)
mse_rf

# RMSE
rmse_rf <- sqrt(mse_rf)


# Varibale Importance
imprf <- h2o.varimp(rf)
imprf

########################################################################
########################################################################
# Lasso
# some best model
cv_lasso   <- cv.glmnet(data_train_x, data_train_y, alpha = 1.0)
min(cv_lasso$cvm)

# predict
pred_l <- predict(cv_lasso, s = cv_lasso$lambda.min, data_test_x)
# Over all mse
mse_lasso <- mean((data_test_y - pred_l)^2)
mse_lasso

# RMSE
rmse_lasso = sqrt(mse_lasso)

# Variable Importance
var_imp1 = varImp(cv_lasso, lambda = cv_lasso$lambda.min)
var_imp1 = as.data.frame(var_imp1)
var_imp1$names <- rownames(var_imp1)
#write_csv(var_imp, "var_imp.csv")

# Most Importance Variables
lassoImportance <- var_imp1[order(var_imp1$Overall, decreasing = TRUE),]

# Variables selection
varsSelected1 <- length(which(lassoImportance$Overall!=0))
varsNotSelected1 <- length(which(lassoImportance$Overall==0))

cat('Lasso uses', varsSelected1, 'variables in its model, and did not select', varsNotSelected1, 'variables.')

#### Top 25 influential variables in Lasso
coef(cv_lasso, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

##


#########################################################################
results <- data.frame(Model = c('Ridge', 'Lasso', 'Random Forest'),
                      MSE = c(mse_ridge, mse_lasso, mse_rf),
                      RMSE = c(rmse_ridge, rmse_lasso, rmse_rf))
results

#results
print("Results for logbid1")
suppressMessages(library(knitr))
kable(results)

# save results in latex
library(stargazer)
stargazer(results, summary = FALSE, type = "latex")

########################################################################
# Load required packages
library(ggplot2)
library(pdp)

# select 15 most important variables to find the partialPlot
data_parplot = subset(data_ebay, select = c(logbid1, miles, 	
                                        logstart, logmiles, 	
                                        startbid, numbids, options, 	
                                        auction, n, cyl, 	
                                        warranty, 	
                                        model, 	
                                        photos,doors,
                                        rust_group, logtext) )

####################################################################
# Random Forest for partial plot using 15 covariates
######################################
set.seed(0)
smp_size = floor(0.98*nrow(data_parplot))
train_ind <- sample(seq_len(nrow(data_parplot)), size = smp_size)

traindata = data_parplot[train_ind, ]
testdata = data_parplot[-train_ind, ]

# RF
rf1 <- randomForest(logbid1 ~ ., traindata, ntree=50, norm.votes=FALSE)

# Using randomForest's partialPlot function
partialPlot(rf1, pred.data = data_train_x, x.var = "cyl")

p1 <- partial(rf1 , pred.var = "cyl", plot = TRUE,
              plot.engine = "ggplot2")

p1

p2 <- partial(rf1 , pred.var = "miles", plot = TRUE,
              plot.engine = "ggplot2")

p2


p3 <- partial(rf1 , pred.var = "startbid", plot = TRUE,
              plot.engine = "ggplot2")

p3

# rust_group, photos, enddate_day
p4 <- partial(rf1 , pred.var = "photos", plot = TRUE,
              plot.engine = "ggplot2")

p4

grid.arrange(p3, p2, p1, p4, ncol = 2)  


















#######################################################################
#######################################################################
# For Sell

rm(list=ls())
# Loading libraries required and reading the data into R
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(tidyverse)
library(haven)
library(haven)
library(lubridate)
library(ggplot2)
library(cowplot)
library(randomForest)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(rsample)  # data splitting 
library(glmnet)   # implementing regularized regression approaches
library(dplyr)    # basic data manipulation procedures
library(ggplot2)  # plotting


# Load Data
setwd("/Users/sachin/Downloads/Econ_725/Projects/code")
df1 <- read_csv("cleaned_data.csv")


# Dim
dim(df1)

# #display first 10 variables and the response variable
str(df1[,c(1:10, 81)]) 

# The response variable; biddy1
ggplot(data=df1[!is.na(df1$biddy1),], aes(x=biddy1)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

# The response variable; logbid1
ggplot(data=df1[!is.na(df1$logbid1),], aes(x=logbid1)) +
  geom_histogram(fill="blue", binwidth = 1) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

# summary
summary(df1$logbid1)

#
df1 <- df1[!(is.na(df1$logbid1)),]
df1 <- df1[!(is.na(df1$photos)),]
df1 <- df1[!(is.na(df1$logmiles)),]
df1 <- df1[!(is.na(df1$logtext)),]

# find most na columns
colSums(is.na(df1))

# delete Na columns
#delete columns
cok=apply(df1,2,function(x)!any(is.na(x)))
df1 = df1[,cok] 

#
#ggplot(data=df1[!is.na(df1$logbid1),], aes(x=factor(sell), y=logbid1))+
#  geom_boxplot(col='blue') + labs(x='Overall Quality') +
#  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)


###################################################################
# For sell

# Correlations with logbid1
numericVars <- which(sapply(df1, is.numeric)) #index vector numeric variables
# numericVarNames <- names(numericVars) #saving names vector for use later on
# cat('There are', length(numericVars), 'numeric variables')

#
data_ebay <- df1[, numericVars]
#cor_numVar <- cor(data_ebay, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with logbid1
#cor_sorted <- as.matrix(sort(cor_numVar[,'logbid1'], decreasing = TRUE))
#select only high corelations
#CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))
#cor_numVar <- cor_numVar[CorHigh, CorHigh]

#corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

# Only sell car
#data_ebay <- data_ebay[data_ebay$sell ==1, ]

# order data first column is sell
col_order <- c(384, 1:383, 385:457)
data_ebay <- data_ebay[, col_order]

# delete biddy1
data_ebay = subset(data_ebay, select = -c(biddy1) )


######################################
set.seed(0)
smp_size = floor(0.98*nrow(data_ebay))
train_ind <- sample(seq_len(nrow(data_ebay)), size = smp_size)

traindata = data_ebay[train_ind, ]
testdata = data_ebay[-train_ind, ]

data_train_x <- model.matrix(sell ~ ., traindata)[, -1]
data_train_y <- traindata$sell

data_test_x <- model.matrix(sell ~ ., testdata)[, -1]
data_test_y <- testdata$sell

dim(data_train_x)

##########################################################################
# Ridge
# some best model
cv_ridge   <- cv.glmnet(data_train_x, data_train_y, alpha = 0)
min(cv_ridge$cvm)

# predict
pred_r <- predict(cv_ridge, s = cv_ridge$lambda.min, data_test_x)
# Over all mse
mse_ridge <- mean((data_test_y - pred_r)^2)
mse_ridge

# RMSE
rmse_ridge <- sqrt(mse_ridge)

########################################################################
# Lasso
# some best model
cv_lasso   <- cv.glmnet(data_train_x, data_train_y, alpha = 1.0)
min(cv_lasso$cvm)

# predict
pred_l <- predict(cv_lasso, s = cv_lasso$lambda.min, data_test_x)
# Over all mse
mse_lasso <- mean((data_test_y - pred_l)^2)
mse_lasso

# RMSE
rmse_lasso <- sqrt(mse_lasso)

#########################################################################
# Random Forest Model
suppressMessages(library(h2o))
suppressMessages(library(randomForest))
suppressMessages(library(neuralnet))
suppressMessages(library(data.table))
suppressMessages(library(stargazer))
suppressMessages(library(glmnet))
suppressMessages(library(knitr))
suppressMessages(library(tidyverse))
set.seed(0)
h2o.init()

rf <- h2o.randomForest(x=2:457, y = 1, training_frame = as.h2o(traindata),
                       validation_frame = as.h2o(testdata), ntree = 50, seed = 420) 

# mse
mse_rf <- h2o.mse(rf)
mse_rf

# RMSE
rmse_rf <- sqrt(mse_rf)


# Varibale Importance
imprf <- h2o.varimp(rf)
imprf


########################################################################

results <- data.frame(Model = c('Ridge', 'Lasso', 'Random Forest'),
                      MSE = c(mse_ridge, mse_lasso, mse_rf),
                      RMSE = c(rmse_ridge, rmse_lasso, rmse_rf))
#results
print("Results for Sell")
suppressMessages(library(knitr))
kable(results)

# save results in latex
library(stargazer)
stargazer(results, summary = FALSE, type = "latex")

########################################################################










