# Things to do:
# 1) Redo the cleaning step by step in a different file
# 2) Create two datasets
# 3) Run the logistic regression model 
# 4) Run the linear reg model for biddy1

library(haven)
library(lubridate)
library(ggplot2)
library(cowplot)
library(randomForest)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

df1 <- read_dta("../files for project/temp for project.dta")
orig_data <- data.frame(df1)



# Remove obviously unnecessary columns (PW)
# -----------------------------------------
orig_data = orig_data[ , -which(colnames(orig_data) %in% c("membersince","name","vin","highbiddername", "itemnum", "location",
                                                           "sellername","questions","X_merge","temp","biddername1","biddername2",
                                                           "biddername3", "biddername4", "biddername5","biddername6","biddername7",
                                                           "biddername8","biddername9","biddername10","biddername11","biddername12",
                                                           "biddername13","biddername14","biddername15","biddername16","biddername17",
                                                           "biddername18","biddername19","biddername20","biddername21","biddername22"))]





# Setting correct variable types
# -------------------------------
## Create a full list of var names for debug
all <- colnames(orig_data)

## A list of var names that are debugged
list <- c()


# -------------------------
# Setting character type - NONE
# -------------------------
# Convert the following to character types


# Add the converted variables to the converted list
list <- c('itemnum', 'location')

# Fish out the remaining dirty variables
all <- all[which(!all %in% list)]
# -----------------------------
# Setting factor type variables
# -----------------------------

# Fishing all the factor types using regex
condition <- c("^ding*","^scratch*","^dent*","^broken*","^crack*","^problem*","^rust*") 

fac_list <- c()

for (i in condition){
  vec <- colnames(orig_data)[grepl(i,colnames(orig_data))]
  fac_list <- append(fac_list,vec)
}

# Adding all the other factor variables manually. # AD: moved caradphotos to integers
fac_list <- c(fac_list,'inspection','relistflag','featured','phone','addedinfo',
              'endsunday','primetime','warranty','relist','sell','dealer',
              'maker','interior','exterior','software', 'address', 'year')  # AD : removed location and added to character.

# Convert all the variables in the 'fac_list' to a factor type
orig_data <- orig_data %>% mutate_at(.vars = fac_list, .funs = as.factor)

list <- append(list,fac_list)

all <- all[which(!all %in% list)]

# -----------------------------
# Setting integer type
# -----------------------------
time <- c("^biddate*","^bidhour*","^bidminute*","^bidsecond*")
int_list <- c()
for (i in time){
  vec <- colnames(orig_data)[grepl(i,colnames(orig_data))]
  int_list <- append(int_list,vec)
}

int_list <- c(int_list,'endhour','endingdate','startingdate','endday','text',
              'miles','numbids','store','pwrseller','highbidderfdback',
              'reserve','buyitnow','sellfdbackpct','photos','descriptionsize','options','doors',  # AD: removed address and year and added to factor
              'trans','webpage','title','condition','model','cyl','length','age','age2','html',
              'sellerborn','week','auction','n','totallisted','totalsold','caradphotos') 

# AD: removed these three variables for now: ,'maxbidtime', 'auc_duration_mins','max_evot')         

orig_data <- orig_data %>% mutate_at(.vars = int_list, .funs = as.integer)
list <- append(list,int_list)
all <- all[which(!all %in% list)]

# What are all the remaining variables?
all

# -----------------------------
# Setting numeric type
# -----------------------------

num_list <- c('bookvalue','pctfdback','startbid','sellerage','negpct','compindex',c(paste0('biddy',1:22)),
              'logmiles','logtext','logsize','logstart','logfdback','logphotos',
              'loghtml','logage','logbook',c(paste0('logbid',1:3)))
orig_data <- orig_data %>% mutate_at(.vars = num_list, .funs = as.numeric)
list <- append(list,num_list)
all <- all[which(!all %in% list)]

# What are all the remaining variables?
all

# Only date variables remain. We use these variables and convert each part into a numeric variable
orig_data$enddate <- mdy_hms(orig_data$enddate, tz=Sys.timezone())
orig_data$startdate <- mdy_hms(orig_data$startdate, tz=Sys.timezone())

# Handling enddate
orig_data$enddate_year <- year(orig_data$enddate)
orig_data$enddate_month <- month(orig_data$enddate)
orig_data$enddate_day <- day(orig_data$enddate)

orig_data$enddate_hour <- hour(orig_data$enddate)
orig_data$enddate_minute <- minute(orig_data$enddate)
orig_data$enddate_second <- second(orig_data$enddate)

orig_data$enddate_wday <- as.factor(wday(orig_data$enddate))


# Handling startdate

orig_data$startdate_year <- year(orig_data$startdate)
orig_data$startdate_month <- month(orig_data$startdate)
orig_data$startdate_day <- day(orig_data$startdate)

orig_data$startdate_hour <- hour(orig_data$startdate)
orig_data$startdate_minute <- minute(orig_data$startdate)
orig_data$startdate_second <- second(orig_data$startdate)

orig_data$startdate_wday <- as.factor(wday(orig_data$startdate))

# Now that end and start dates are handled, we add them to the list
list <- c(list,"startdate", "enddate")
list

# Removing these two PosixCt items from the list
orig_data <- subset(orig_data, select = -c(enddate, startdate))

# Removing objects created during the process of datacleaning


dim(orig_data)
glimpse(orig_data)
saveRDS(orig_data, "../files for project/cleaned_data")

rm(all,i,list,num_list,time,vec, condition, fac_list, int_list, df1, orig_data) ##Remove useless objects

# ------------------------
# Running a RF model
# ------------------------
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
colnames(df3)

# Now, we will choose only the variables that can be pre-determined before the car is sold or listed on the auction

features <- c("maker", "text", "interior", "exterior", "inspection", "miles", "phone", "address", "store", "buyitnow", "photos",
              "addedinfo", "descriptionsize", "ding_two", "ding_tiny", "ding_detectable", "ding_few", "scratch_two", "scratch_tiny", 
              "scratch_detectable", "scratch_few", "dent_small", "dent_visible", "dent_two", "dent_tiny", "dent_detectable", 
              "dent_few", "broken_two", "broken_tiny", "broken_detectable", "broken_few", "crack_wide", "crack_large", "crack_negligible", 
              "crack_two", "crack_tiny", "crack_detectable", "crack_few", "crack_medium", "problem_one", "problem_two", "problem_tiny",
              "problem_detectable", "problem_few", "rust_two", "rust_tiny", "rust_detectable", "rust_few", "doors", "trans", "webpage",
              "condition", "model", "cyl", "warranty", "age", "age2", "logmiles", "logtext","software", "caradphotos", "totallisted",
              "dealer", "ding_bad", "ding_knowledge", "ding_pics", "dent_knowledge", "dent_pics", "crack_knowledge", 
              "crack_pics", "problem_bad", "problem_knowledge", "problem_pics", "rust_knowledge", "rust_pics", "scratch_knowledge", 
              "scratch_pics", "broken_bad", "broken_knowledge", "broken_pics", "ding_group", "scratch_group", "crack_group", "broken_group", 
              "dent_group", "problem_group", "rust_group", "negpct", "startdate_year","startdate_month", "startdate_day", "startdate_hour", 
              "startdate_minute", "startdate_second", "startdate_wday")

doubtful <- c("featured", "pwrseller", "highbidderfdback", "reserve", "sellfdbackpct", "options", "pctfdback", "title", 
              "logsize", "logstart", "logfdback", "logphotos", "html", "loghtml", "sellerborn", "sellerage", "logage" )

# Set details of the dataset
set.seed(100)
rows <- 1000

dfx <- df3[sample(nrow(df3), rows), unique(c("sell", features))]
colnames(dfx)

#Imputing data
dfx.impute <- rfImpute(sell ~ ., data = dfx, iter = 6)

# Vector memory reached. Not functioning

# ---------------------------------
# Running regressions
# ---------------------------------

# For us to run the regressions with all valid vars, the vectors that contain only one type of
# value is causing a problem. So, we will have to eliminate all the variables that contain only
# one type of value

# take all the variable names in the dataset
names <- colnames(df2)
names
length(names)
# There are 587 variables

# Store the length of unique values in each variable
length <- 9999
vars_len <- data.frame(names, length)
head(vars_len)

for(i in 1:nrow(vars_len)){
  vars_len$length[i] <- length(unique(df1[,i]))
}

head(vars_len)
vars_len

# Remove all variables with length == 1
vars_len <- vars_len[(vars_len$length > 1),]
nrow(vars_len)
# We have retained 499 variables. These are the variables that have more than one type of observation

df3 <- df2[ ,c(vars_len$names)]
colnames(df3)

dfx <- df3[,c("sell", "exterior", colnames(df3[,6:10]))]
colnames(dfx)

mod1 <- glm(sell ~ ., data = dfx, family = "binomial")
summary(mod1)


# Fit the full model 
full.model <- glm(sell ~., data = dfx, family = "binomial")
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)



summary(dfx)



df_factor <- df3 %>% select_if(is.factor)
df_numeric <- df3 %>% select_if(is.numeric)
#df_int <- data %>% select_if(is.integer)
df4 <- cbind(df3$sell, df_numeric)
colnames(df4)

str(df4)


str(df2,list.len=ncol(df2))


df3 <- subset(df2, select = -c(maker, model, ding_enormous, ding_seldom, ding_negligible, ding_detectable, ding_sizable, ding_vast, ding_enormous,
                               scratch_enormous, scratch_seldom, scratch_detectable, scratch_sizable, scratch_vast, 
                               dent_enormous, dent_seldom, dent_negligible, dent_detectable, dent_sizable, dent_vast, 
                               broken_wide, broken_enormous, broken_seldom, broken_minute, broken_negligible, broken_limited,
                               broken_substantial, broken_rarely, broken_detectable, broken_medium, broken_significant, broken_invisible,
                               broken_sizable, broken_vast, crack_enormous, crack_seldom, crack_minute, crack_negligible,
                               crack_substantial, crack_detectable, crack_invisible, crack_sizable, crack_vast,
                               problem_enormous, problem_seldom, problem_negligible, problem_substantial, problem_detectable,
                               problem_invisible, problem_sizable, rust_enormous, rust_negligible, rust_detectable, rust_invisible, rust_vast, software))


str(df3,list.len=ncol(df3))
mod1 <- glm(sell ~ ., data = df3, family = "binomial")
mod1

df3 <- na.omit(df3)
sapply(df3, levels)

dat <- df1
subset_vec <- df3

# 1. What determines if the car is sold? (sell = 1)
# -----------------------------------------------------------------------------
if (mode(subset_vec) == "logical") {
  if (length(subset_vec) != nrow(dat)) {
    stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
  }
  subset_log_vec <- subset_vec
} else if (mode(subset_vec) == "numeric") {
  ## check range
  ran <- range(subset_vec)
  if (ran[1] < 1 || ran[2] > nrow(dat)) {
    stop("'numeric' `subset_vec` provided but values are out of bound")
  } else {
    subset_log_vec <- logical(nrow(dat))
    subset_log_vec[as.integer(subset_vec)] <- TRUE
  } 
} else {
  stop("`subset_vec` must be either 'logical' or 'numeric'")
}
dat <- base::subset(dat, subset = subset_log_vec)








# 2. What determines if the bids are placed?
# -----------------------------------------------------------------------------


# Assume that for a seller to sell the car, at least one bid has to be placed. 
# In the absence of a reserve price, this determines whether the car is sold or not. 
# In the dataset, we do not have information on what is the reserve price, 
# but simply on whether a reserve price exists or not. So we go under the assumption that the 
# reserve price is sufficiently small that even the smallest bid ends up selling the car. 

# Question: Do we know what is the reserve price? 

table(df1$numbids > 0, df1$reserve)

str(df1,list.len=ncol(df1))
?str
df1$sold <- 0
df1[df1$numbids > 0, ]$sold <- 1

df2 <- subset(df1, select = -c(rust_vast,rust_invisible,rust_detectable,rust_negligible,rust_enormous,problem_sizable,
                               problem_invisible,problem_detectable,problem_substantial,problem_negligible,problem_seldom,
                               problem_enormous,crack_vast, crack_sizable, crack_invisible, crack_detectable, crack_substantial,
                               crack_negligible, crack_minute, crack_seldom, crack_enormous, broken_vast, broken_sizable,
                               broken_invisible, broken_significant, broken_medium, broken_detectable, broken_rarely, broken_substantial,
                               broken_limited, broken_negligible, broken_minute, broken_seldom, broken_enormous, broken_wide, dent_vast,
                               dent_sizable, dent_detectable, dent_negligible, dent_seldom, dent_enormous,scratch_vast, scratch_sizable, 
                               scratch_detectable, scratch_seldom, scratch_enormous, ding_vast, ding_sizable, ding_detectable,
                               ding_negligible, ding_seldom,ding_enormous,start_s, end_s, peak_s,season_trans))
str(df2, list.len=ncol(df2))

mod1 <- lm(sold ~ ., data = df2, family = "binomial")

mod1
summary(df2$location)

table(df1$sold)

df2 <- df1 %>%
  group_by(year) %>%
  summarise(numbids = mean(numbids))

df2
plot(df1$year, df1$numbids)
plot(df2$year, df2$numbids)

colname_df2 <- colnames(df2)
summary(eval(paste0("df2$", colname_df2[478])))

get(paste0("df2$", colname_df2[1]))
get("df2$crack")

df2$crack

for(i in 1:10){
  summary(eval(as.name(paste0("df2$", colname_df2[3]))))
}

eval(as.symbol(paste0('df2$', colnames(df2[3]))))
summary(df2$interior)

# 2. Predictive model for the revenue of the car.
# ---------------------------------------------
# What determines the highest bid? 
