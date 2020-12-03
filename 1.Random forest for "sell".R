# This file will be used to test and perfect the RF model to predict "sell"
#install.packages("devtools")
#library(devtools)
#devtools::install_github('araastat/reprtree')
library(caret)
library(e1071)
library(reprtree)
library(tidyverse)
library(data.table)
library(randomForest)
library(ggplot2)
# ---------------------------------
# 1. Find the most frequent car and model
# ---------------------------------
data <- data.frame(readRDS("../files for project/cleaned_data"),na.strings = c("NA"))
sort(table(data$model))

# We see that the most frequent maker is Ford (some car within ford that is converted to a factor and unable to read)
# So we subset the dataset by Ford (some car within ford that is converted to a factor and unable to read)

df2 <- data[data$model == "39", ]

100*nrow(df2)/nrow(data)
# We get 30k observations, which is about 20% of the data

# ------------------------------------------------
# 2. Subsetting the data for only the derived variables
# ------------------------------------------------
# Lets remove the raw variables, and play with only the ones derived by the author

raw <- c("ding_barely", "ding_minute", "ding_negligible", "ding_small", "ding_limited", "ding_almost", "ding_minor", "ding_little", "ding_invisible",
         "ding_wide", "ding_enormous", "ding_noticeable", "ding_large", "ding_obvious", "ding_major", "ding_substantial", "ding_visible", "ding_huge", "ding_medium", "ding_big", "ding_significant", "ding_sizable", "ding_vast",
         "ding_apparent", "ding_known", "ding_no", "ding_free", "ding_never", "ding_nothing", "ding_seldom", "ding_one", "ding_rarely", "ding_only", "ding_hardly", "ding_couple",
         "ding_several", "ding_much", "ding_very", "ding_extremely", "ding_many", "ding_some", "ding_pic", "ding_photo", "dent_barely", "dent_minute", "dent_negligible", "ding_small", "dent_limited", "dent_almost", "dent_minor", "dent_little", "dent_invisible",
         "dent_wide", "dent_enormous", "dent_noticeable", "dent_large", "dent_obvious", "dent_major", "dent_substantial", "dent_huge", "dent_medium", "dent_big", "dent_significant", "dent_sizable", "dent_vast",
         "dent_apparent", "dent_known", "dent_no", "dent_free", "dent_never", "dent_nothing", "dent_seldom", "dent_one", "dent_rarely", "dent_only", "dent_hardly", "dent_couple",
         "dent_several", "dent_much", "dent_very", "dent_extremely", "dent_many", "dent_some", "dent_pic", "dent_photo", 
         "crack_barely", "crack_minute", "crack_small", "crack_limited", "crack_almost", "crack_minor", "crack_little", "crack_invisible",
         "crack_enormous", "crack_noticeable", "crack_obvious", "crack_major", "crack_substantial", "crack_visible", "crack_huge", "crack_big", "crack_significant", "crack_sizable", "crack_vast",
         "crack_apparent", "crack_known", "crack_no", "crack_free", "crack_never", "crack_nothing", "crack_seldom", "crack_one", "crack_rarely", "crack_only", "crack_hardly", "crack_couple", 
         "crack_several", "crack_much", "crack_very", "crack_extremely", "crack_many", "crack_some", "crack_pic", "crack_photo", 
         "problem_barely", "problem_minute", "problem_negligible", "problem_small", "problem_limited", "problem_almost", "problem_minor", "problem_little", "problem_invisible", 
         "problem_wide", "problem_enormous", "problem_noticeable", "problem_large", "problem_obvious", "problem_major", "problem_substantial", "problem_visible", "problem_huge", "problem_medium", "problem_big", "problem_significant", "problem_sizable", "problem_vast",
         "problem_apparent", "problem_known", "problem_no", "problem_free", "problem_never", "problem_nothing",
         "problem_seldom", "ding_one", "problem_rarely", "problem_only", "problem_hardly", "problem_couple", "problem_several", "problem_much", "problem_very", "problem_extremely", "problem_many", "problem_some",
         "problem_pic", "problem_photo", "rust_barely", "rust_minute", "rust_negligible", "rust_small", "rust_limited", "rust_almost", "rust_minor", "rust_little", "rust_invisible",
         "rust_wide", "rust_enormous", "rust_noticeable", "rust_large", "rust_obvious", "rust_major", "rust_substantial", "rust_visible", "rust_huge", "rust_medium", "rust_big", "rust_significant", "rust_sizable", "rust_vast",
         "rust_apparent", "rust_known","rust_no", "rust_free",  "rust_never", "rust_nothing", "rust_seldom", "rust_one", "rust_rarely", "rust_only", "rust_hardly", "rust_couple",
         "rust_several", "rust_much", "rust_very", "rust_extremely", "rust_many", "rust_some", "rust_pic", "rust_photo", "scratch_barely", "scratch_minute", "scratch_negligible", "scratch_small", "scratch_limited", "scratch_almost", "scratch_minor", "scratch_little", "scratch_invisible",
         "scratch_wide", "scratch_enormous", "scratch_noticeable", "scratch_large", "scratch_obvious", "scratch_major", "scratch_substantial", "scratch_visible", "scratch_huge", "scratch_medium", "scratch_big", "scratch_significant", "scratch_sizable", "scratch_vast",
         "scratch_apparent", "scratch_known", "scratch_no", "scratch_free", "scratch_never", "scratch_nothing", "scratch_seldom", "scratch_one", "scratch_rarely", "scratch_only", "scratch_hardly", "scratch_couple",
         "scratch_several", "scratch_much", "scratch_very", "scratch_extremely", "scratch_many", "scratch_some", "scratch_pic", "scratch_photo",
         "broken_barely", "broken_minute", "broken_negligible", "broken_small", "broken_limited", "broken_almost", "broken_minor", "broken_little", "broken_invisible",
         "broken_wide", "broken_enormous", "broken_noticeable", "broken_large", "broken_obvious", "broken_major", "broken_substantial", "broken_visible", "broken_huge", "broken_medium", "broken_big", "broken_significant", "broken_sizable", "broken_vast",
         "broken_apparent", "broken_known", "broken_no", "broken_free",  "broken_never", "broken_nothing", "broken_seldom", "broken_one", "broken_rarely", "broken_only", "broken_hardly", "broken_couple",
         "broken_several", "broken_much", "broken_very", "broken_extremely", "broken_many", "broken_some", "broken_pic", "broken_photo", "ding",
         "ding_negation", "ding_good", "ding_low", "ding_high", "scratch", "scratch_negation", "scratch_good", "scratch_low", "scratch_high", "scratch_bad",
         "crack", "crack_negation", "crack_good", "crack_low", "crack_high", "crack_bad", "broken", "broken_negation", "broken_good", "broken_low", 
         "broken_high", "dent", "dent_negation", "dent_good", "dent_low", "dent_high", "dent_bad", "problem", "problem_negation", "problem_good", "problem_low",
         "problem_high", "rust", "rust_negation", "rust_good", "rust_low", "rust_high", "rust_bad")

df3 <- df2[ , !(names(df2) %in% raw)]

# ------------------------------------------------
# 3. Categorize remaining variables as per their source 
# ------------------------------------------------

# Our dataset contains a number of variables that can predict whether the car is sold or not. 
# But we cannot throw all of them into the model. We must be careful to understand
# what are the variable that are indeed under the control of the seller and the auction house
# before they decide to choose to sell the model. This will allow us to predict whether the 
# car can be sold or not.

# Notes: 
# 1) Photos has a value of -1. Need to understand what that is.
# 2) Condition has barely three values. need to see what that is.
# 3) Need to convert seller born to a correct variable
# 4) Removed year from 'car description' because it wouldnt accept factors > 54. Need to reintroduce it

car_description <- c("maker","interior", "exterior","miles", "inspection", "doors", "trans", "model", "cyl", "warranty", "age", "age2")
listing_features <- c("text","phone", "address","store", "buyitnow", "photos","addedinfo", "descriptionsize","webpage",
                      "caradphotos", "totallisted", "title","html","featured", "reserve", "auction", "primetime", "relist")
auction_time <- c("startdate_year","startdate_month", "startdate_day", "startdate_hour", 
                  "startdate_minute", "startdate_second", "startdate_wday",
                  "enddate_year", "enddate_month", "enddate_day","enddate_hour",
                  "enddate_minute", "enddate_second", "enddate_wday", "length", "week")
car_quality <- c("ding_two", "ding_tiny", "ding_detectable", "ding_few", "scratch_two", "scratch_tiny", 
                 "scratch_detectable", "scratch_few", "dent_small", "dent_visible", "dent_two", "dent_tiny", "dent_detectable", 
                 "dent_few", "broken_two", "broken_tiny", "broken_detectable", "broken_few", "crack_wide", "crack_large", "crack_negligible", 
                 "crack_two", "crack_tiny", "crack_detectable", "crack_few", "crack_medium", "problem_one", "problem_two", "problem_tiny",
                 "problem_detectable", "problem_few", "rust_two", "rust_tiny", "rust_detectable", "rust_few","ding_bad", "ding_knowledge", "ding_pics", "dent_knowledge", "dent_pics", "crack_knowledge", 
                 "crack_pics", "problem_bad", "problem_knowledge", "problem_pics", "rust_knowledge", "rust_pics", "scratch_knowledge", 
                 "scratch_pics", "broken_bad", "broken_knowledge", "broken_pics", "ding_group", "scratch_group", "crack_group", "broken_group", 
                 "dent_group", "problem_group", "rust_group","condition")
log_variables <- c("logmiles", "logtext","logsize", "logstart", "logfdback", "logphotos","logage","loghtml")
seller_features <- c("software","dealer",  "negpct","sellerborn", "sellerage","pwrseller")


# --------------------------------------------------------
# 4. Create a dataframe of variable and their type from above
# ----------------------------------------------------------

# Tweak this section for the variables that you want included:
types <- data.frame(vars = c("car_description", 
                             "listing_features",
                             "auction_time", 
                             "car_quality", 
                             "log_variables", 
                             "seller_features"))

vars <- NULL
temp <- NULL
for(i in 1:nrow(types)){
  temp <- data.frame(variable = eval(parse(text = types$vars[i])),
                     category = types$vars[i])
  
  vars <- data.frame(rbind(vars, temp))
}
vars

# --------------------------------------------------------
# 5. Preparing training and testing datasets
# ----------------------------------------------------------
set.seed(100)
nrow(data)
# We have 30,000 rows of data

data <- df3[, unique(c("sell", vars$variable))]

# Let us try to impute the missing values with "sell" as the DV
# Imputing data
data.impute <- rfImpute(sell ~ ., data = data, iter = 6)
# Too large memory. Unable to do so.

# Let us keep only 5000 rows, and randomly select observations for the dataset
rows <- 5000
data <- df3[sample(nrow(df3), rows), unique(c("sell", vars$variable))]

# Imputing data (need to rethink whether impute is necessary)
data.impute <- rfImpute(sell ~ ., data = data, iter = 6)


# Change levels of the 'Sell' variable to make it easier on the eyes
levels(data.impute$sell) <- list(unsold = "0", sold = "1")

# Data Partition
set.seed(100)
key <- sample(2, nrow(data.impute), replace = TRUE, prob = c(0.7, 0.3))
train <- data.impute[key==1, ]
test <- data.impute[key==2, ]

# --------------------------------------------------------
# 6. Creating RF models to predict "sell"
# ----------------------------------------------------------

model_1 <- randomForest(sell ~ ., data = train,
                        proximity = TRUE,
                        mtry = 40,
                        ntree = 500,
                        importance = TRUE)
model_1

# reprtree:::plot.getTree(model_1)

# Prediction & Confusion Matrix - train data
p1 <- predict(model_1, train)
confusionMatrix(p1, train$sell)

# We see that the prediction on the training dataset is perfect! 

# # Prediction & Confusion Matrix - test data
p2 <- predict(model_1, test)
confusionMatrix(p2, test$sell)

# We see that the accuracy is ~95% on the testing dataset!!! 

# Error rate of Random Forest
plot(model_1)


# Tune mtry
t <- tuneRF(subset(train, select = -c(sell)), train[,c("sell")],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 500,
            trace = TRUE,
            improve = 0.05)

# We see that the minimum OOB error is for a nodesize of 40

# No. of nodes for the trees
hist(treesize(model_1),
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance
varImpPlot(model_1,
           sort = T,
           n.var = 20,
           main = "Top 10 - Variable Importance")

# Quantitative measures of the importance accorded to variables
importance(model_1)

# How many times have the variables from the above table been used to predict the model
varUsed(model_1)

# Partial Dependence Plot
partialPlot(model_1, train, reserve, "unsold")

# Extract Single Tree
getTree(model_1, 1, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(model_1, train$sell)

# ------------------


# Finding optimal number of trees
oob.error.rate <- data.frame(
  Trees = rep(1:nrow(model_1$err.rate), times = 3),
  Type = rep(c("OOB", "Unsold", "Sold"), each = nrow(model_1$err.rate)),
  Error = c(model_1$err.rate[, "OOB"],
            model_1$err.rate[, "unsold"],
            model_1$err.rate[, "sold"]))

oob.error.rate

ggplot(data = oob.error.rate, aes(x = Trees, y = Error)) +
  geom_line(aes(color=Type))

# Finding optimal number of variables at each internal node

oob.values <- vector(length = 20)
for(i in 1:length(oob.values)){
  temp.model <- randomForest(sell ~ ., data = train, mtry = i, ntree = 500)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}

which.min(oob.values)
plot(oob.values)


# The next question is whether this model accuracy is good enough 

# https://www.youtube.com/watch?v=dJclNIN-TPo

# Plot the proximity matrix
model$terms
plot(model)
varImpPlot(model)
dfx.impute$totalsold


#-----------------------------
var <- importance(model_1) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

var <- var[order(var$MeanDecreaseGini,decreasing = TRUE),] 
var <- var$feature[1:20]  ##most important 20 vars

# Next we run k-means clustering
# Using the elbow method to find the optimal number of clusters
#install.packages('factoextra')
library(factoextra)

df2$maker <- factor(df2$maker,level=unique(df2$maker),labels=1:length(unique(df2$maker)),exclude = NULL)
# df2$model<- factor(df2$model,level=unique(df2$model),labels=1:length(unique(df2$model)),exclude = NULL)
df2$interior <- factor(df2$interior,level=unique(df2$interior),labels=1:length(unique(df2$interior)),exclude = NULL)
df2$exterior <- factor(df2$exterior,level=unique(df2$exterior),labels=1:length(unique(df2$exterior)),exclude = NULL)
#nndata$location <- factor(nndata$location,level=unique(nndata$location),labels=1:length(unique(nndata$location)),exclude = NULL)
#nndata$software <- factor(nndata$software,level=unique(nndata$software),labels=1:length(unique(nndata$software)),exclude = NULL)
df2$caradphotos <- factor(df2$caradphotos,level=unique(df2$caradphotos),labels=1:length(unique(df2$caradphotos)),exclude = NULL)


df2$sold <- 0
df2$sold[which(df2$numbids > 0)] <- 1
  
df4 <- df2[c(listing_features,car_description,'biddy1','sold')] 
df4$biddy1[which(df4$sold==0)] <- 0

df4 <- data.frame(lapply(df4,as.numeric))
df4 <- na.omit(df4)

x <- df4[,which(!names(x) %in% c('sold','biddy1','model','maker'))]
x <- as.data.frame(scale(x))

set.seed(100)
wcss = vector()
for (i in 1:6) wcss[i] = sum(kmeans(x, i)$withinss,na.rm=TRUE)
plot(1:6,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of cluster',
     ylab = 'WCSS')

fviz_nbclust(x, kmeans, method = "wss")


# Fitting K-Means to the dataset
set.seed(100)
kmeans = kmeans(x, 3, iter.max =300, nstart =200)
y_kmeans = kmeans$cluster

df4$cluster <- y_kmeans

install.packages('tl;dr')
sum <- df4 %>% group_by(cluster) %>% summarise_all(mean,na.rm = TRUE)
data.frame(sum)

x %>%
  as_tibble() %>%
  mutate(cluster = kmeans$cluster) %>%
  ggplot(aes(photos, text, color = factor(cluster), label = cluster)) +
  geom_text()


#--------------------------------------
nndata <- data.frame(readRDS("../files for project/cleaned_data"))
nndata$sold <- 0
nndata$sold[which(nndata$numbids > 0)] <- 1
nndata <- nndata[c(var,'sold')]

#nndata$maker <- factor(nndata$maker,level=unique(nndata$maker),labels=1:length(unique(nndata$maker)),exclude = NULL)
#nndata$model<- factor(nndata$model,level=unique(nndata$model),labels=1:length(unique(nndata$model)),exclude = NULL)
nndata$interior <- factor(nndata$interior,level=unique(nndata$interior),labels=1:length(unique(nndata$interior)),exclude = NULL)
nndata$exterior <- factor(nndata$exterior,level=unique(nndata$exterior),labels=1:length(unique(nndata$exterior)),exclude = NULL)
#nndata$location <- factor(nndata$location,level=unique(nndata$location),labels=1:length(unique(nndata$location)),exclude = NULL)
nndata$software <- factor(nndata$software,level=unique(nndata$software),labels=1:length(unique(nndata$software)),exclude = NULL)
#nndata <- nndata[which(nndata$software!=1 & nndata$software!=2),]

nndata <- data.frame(lapply(nndata,as.numeric))
nndata <- na.omit(nndata)


# \Set test and train data set
n <- dim(nndata[1])
set.seed(0)
train.id <- sample.int(n, floor(0.8*n), replace = F)

train <- nndata[train.id,]
test <- nndata[-train.id,]
train.x <- data.frame(lapply(train[names(train)!='sold'],
                  function(x) if(is.numeric(x)){
                    scale(x, center=TRUE, scale=TRUE)
                  } else x))
test.x <- data.frame(lapply(test[names(test)!='sold'],
                 function(x) if(is.numeric(x)){
                   scale(x, center=TRUE, scale=TRUE)
                 } else x))

# Fitting ANN to the Training set
# install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)
classifier = h2o.deeplearning(y = 'sold',
                              training_frame = as.h2o(train),
                              activation = 'Rectifier',
                              hidden = c(64,32,16),
                              epochs = 100)


nn_mse <- h2o.mse(classifier)

prob_pred = h2o.predict(classifier, newdata = as.h2o(test))
y_pred = (prob_pred > 0.5)
y_pred = as.vector(y_pred)
y_pred
summary(y_pred)

check <- fifelse(y_pred==test$sold,1,0)
mean(check)

##CAP Test
test %>% 
  mutate(PD = y_pred, BAD = case_when(sold == 1 ~ "bad", TRUE ~ "good")) %>% 
  select(BAD, PD) %>% 
  mutate(Group = cut(y_pred, breaks=10)) %>% 
  group_by(Group, BAD) %>% 
  count() %>% 
  ungroup() %>% 
  spread(key = "BAD", value = "n") %>% 
  mutate(good = replace_na(good, 0)) %>% 
  mutate(RankRisk = 1:nrow(.)) %>% 
  arrange(-RankRisk) %>% 
  mutate(Total_gr = bad + good) %>% 
  mutate(Total_obs = sum(Total_gr)) %>% 
  mutate(SoldRate = bad / sum(bad)) %>% 
  mutate(SoldRateCum = cumsum(SoldRate)) %>% 
  mutate(Per_gr = Total_gr / Total_obs)  %>% 
  mutate(Per_cum = cumsum(Per_gr)) %>% 
  mutate(Rate = bad / Total_gr) -> df_cap


df_cap %>% 
  ggplot(aes(Per_cum, SoldRateCum)) + 
  geom_line(color = "blue", size = 1.3) + 
  geom_point(color = "red", size = 2) + 
  labs(x = NULL, 
       title = "CAP for NNModel") +
  scale_y_continuous(labels = scales::percent)

