rm(list = ls())
source('0. Data cleaning.R')


var <- colnames(orig_data)
orig_data$model <- factor(orig_data$model)
orig_data <- orig_data %>% group_by(model) %>% arrange(model)


# \Find the most frequently observed model
which.max(summary(orig_data$model))
## The most frequently observed one is:
id.top <- which.max(summary(orig_data$model))
unique(orig_data$model)[id.top] ## model 39 with 30258 obs


# \Generate Winning bid
biddy_list <- sprintf("biddy%d",1:22)  ## vector with formatted combination of text and variable values
my.max <- function(x){
  ifelse(!all(is.na(x)), max(x, na.rm=T), NA)
}  ## set my own max function to avoid '-INF'

biddy <- orig_data %>% ungroup() %>% 
  select(biddy_list) 
biddy <- biddy %>% mutate(winningbid=apply(biddy,1,FUN=my.max)) %>% 
  mutate(winningbid=na_if(winningbid,NA)) 

biddy <- orig_data %>% add_column(winningbid=biddy$winningbid)
biddy <- biddy[,!names(biddy) %in% biddy_list]


# \Gather predictors
predictors <- var[!var %in% biddy_list]  # remove biddy1~biddy22

time <- c("^biddate*","^bidhour*","^bidminute*","^bidsecond*")
time_list <- c()
for (i in time){
  vec <- colnames(orig_data)[grepl(i,colnames(orig_data))]
  time_list <- append(time_list,vec)
}
predictors <- predictors[!predictors %in% time_list]  # remove bid time variables

condition <- c("^ding*","^scratch*","^dent*","^broken*","^crack*","^problem*","^rust*")  #list all condition relevant vars
cond_list <- c()
for (i in condition){
  vec <- colnames(orig_data)[grepl(i,colnames(orig_data))]
  cond_list <- append(cond_list,vec)
}
predictors <- predictors[!predictors %in% cond_list]

useless_list <- c('enddate','startdate',
                  'start_m','end_m','start_s','end_s','peak_s','season_trans')
one_level <- c('ding_enormous','ding_seldom','ding_negligible','ding_detectable','ding_sizable','ding_vast',
               'scratch_enormous','scratch_seldom')
predictors <- predictors[!predictors %in% useless_list]  # remove other useless variables
predictors <- predictors[!predictors %in% one_level]
predictors <- c(predictors,'winningbid')
nndata <- subset(biddy, select=predictors) 

nndata$maker <- factor(nndata$maker,level=unique(nndata$maker),labels=1:length(unique(nndata$maker)),exclude = NULL)
nndata$interior <- factor(nndata$interior,level=unique(nndata$interior),labels=1:length(unique(nndata$interior)),exclude = NULL)
nndata$exterior <- factor(nndata$exterior,level=unique(nndata$exterior),labels=1:length(unique(nndata$exterior)),exclude = NULL)
nndata$location <- factor(nndata$location,level=unique(nndata$location),labels=1:length(unique(nndata$location)),exclude = NULL)
nndata$software <- factor(nndata$software,level=unique(nndata$software),labels=1:length(unique(nndata$software)),exclude = NULL)
nndata$caradphotos <- factor(nndata$caradphotos,level=unique(nndata$caradphotos),labels=1:length(unique(nndata$caradphotos)),exclude = NULL)
# The nndata is a full data set including potentially useful predictors
#nndata <- nndata %>% drop_na()




# \Set test and train data set
n <- dim(nndata[1])
set.seed(0)
train.id <- sample.int(n, floor(n/2), replace = F)

train <- nndata[train.id,]
test <- nndata[-train.id,]
train.x <- lapply(train[names(train)!='winningbid'],
                  function(x) if(is.numeric(x)){
                    scale(x, center=TRUE, scale=TRUE)
                    } else x)
test.x <- lapply(test[names(test)!='winningbid'],
                 function(x) if(is.numeric(x)){
                   scale(x, center=TRUE, scale=TRUE)
                 } else x)

# Fitting ANN to the Training set
# install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)
classifier = h2o.deeplearning(y = 'winningbid',
                              training_frame = as.h2o(train),
                              validation_frame = as.h2o(test),
                              activation = 'Rectifier',
                              hidden = c(32,16),
                              epochs = 10)
# This runs really slow







``