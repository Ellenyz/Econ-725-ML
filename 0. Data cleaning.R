# This is the script that we will use for data cleaning. 
# ----------------
# Instructions
# ----------------
# 1. Please navigate to the 'datacleaning' branch when making changes to this file. 
# 2. Please commit every small change and give a clear description of the change in the commit message
# 3. Please comment on the code as clearly as possible
# --------------------------



library("haven")
library(tidyverse) #includes the dplyr and tidyr packages
library(dplyr)
library(tidyr)

#setwd('C:/Users/Sachin/Desktop/my_725_project_work')
# Importing the dataset

# df1 <- read_dta("temp for project.dta")
orig_data <- data.frame(df1)

dim(orig_data)
# We have 146734 rows and 605 variables. 

# Remove obviously unnecessary columns (PW)
# -----------------------------------------
orig_data = orig_data[ , -which(colnames(orig_data) %in% c("membersince","name","vin","highbiddername",
                                                           "sellername","questions","X_merge","temp","biddername1","biddername2",
                                                           "biddername3", "biddername4", "biddername5","biddername6","biddername7",
                                                           "biddername8","biddername9","biddername10","biddername11","biddername12",
                                                           "biddername13","biddername14","biddername15","biddername16","biddername17",
                                                           "biddername18","biddername19","biddername20","biddername21","biddername22"))]

dim(orig_data)
# We have 146734 rows and 575 variables (We removed 30 variables).

# Fixing date and time date and time (JL)
# --------------------------------------
## generally, two types of datetime vars here: standard and numeric
## turn standard to time var:

orig_data$enddate = strptime(orig_data$enddate, "%b-%d-%y %H:%M:%S")
orig_data$startdate = strptime(orig_data$startdate, 
                               "%b-%d-%y %H:%M:%S")
### auction last in mins
orig_data$auc_duration_mins = as.numeric(difftime(orig_data$enddate,
                                                  orig_data$startdate,
                                                  units="mins"))

# Note: 
#   1) The var "length" is auction lasts in days does the same thing
#   2) Note that any var that link "bid" and "1" (eg. logbid1) relates to the highest bid

# Deal with nemeric time var:

### Calculate how long it takes for the highest bidder to make decision (in days)

orig_data$maxbidtime = orig_data$biddate1 - orig_data$startingdate

# Note: I think bidhour, bidminute, bidsec, etc should not be necessary as the best way to
#     handle time is to calculate duration i.e. time difference it doesn't make sense to 
#     include a numeric value i.e. bidhour

# 1. Use the "enddate", "startdate" to calculate month indicator (1,...,12)

orig_data$start_m = match(substr(orig_data$startdate, 1, 3), month.abb)
orig_data$end_m = match(substr(orig_data$enddate, 1, 3), month.abb)

# 2. Use month indicator to calculate season

# ERROR: attach(orig_data)
orig_data$start_s <- ifelse(start_m==12 | start_m==1 | start_m==2, "Winter", 
                            ifelse(start_m==3 | start_m==4 | start_m==5, "Spring", 
                                   ifelse(start_m==6 | start_m==7 | start_m==8, "Summer",
                                          ifelse(start_m==9 | start_m==10 | start_m==11, "Fall", 
                                                 NA))))

orig_data$end_s <- ifelse(end_m==12 | end_m==1 | end_m==2, "Winter", 
                          ifelse(end_m==3 | end_m==4 | end_m==5, "Spring", 
                                 ifelse(end_m==6 | end_m==7 | end_m==8, "Summer",
                                        ifelse(end_m==9 | end_m==10 | end_m==11, "Fall", 
                                               NA))))

# 2a. Create dummy indicating if auction happened in car-demand peak season
orig_data$peak_s = ifelse(orig_data$start_s=="Spring" | orig_data$start_s=="Fall",
                          1, 0)

# 2b. Create dummy indicating if start and end season are different
orig_data$season_trans = ifelse(orig_data$start_s!=orig_data$end_s,
                                1, 0)

# 3. Var "length" is auction lasts in days, all set
# 4. Var "endday" is weekday indicator (1,...,7), all set
# 5. Var "endhour" is which the hour (24hr-clock) the auction ends, all set
# 6. Var "endsunday" is a dummy pointing if endday is Sunday, all set

# 7. Among "biddate1" "bidhour1" "bidminute1" "bidsecond1":
## 7a. Calculate how long it took for the highest bid to appear
orig_data$max_evot = orig_data$biddate1 - orig_data$startingdate

# 7Note: All others from "biddate2" to "bidsecond22"...are no longer useful here 



# Setting correct variable types
# -------------------------------
## Create a full list of var names 
all <- colnames(orig_data)
all
## A list of var names with good types
list <- c()

# -------------
# Working on character variables
df2 <- orig_data[, sapply(orig_data, class) == 'character']

# AD: These variables need to be fixed from character to date-time type
## Date <- c('membersince','enddate','startdate')  These date vars are in type of chars by default

chr <- c(colnames(df2),'sellerid') # vector of all chr names
list <- append(list,chr)
# SOME PROBLEM -----> df2 <- cbind(df2,as.character(orig_data['sellerid']))

orig_data <- orig_data[,-which(colnames(orig_data) %in% list)] ##update data set unchecked

## After checking, all chr variables have been set as default. df2 is the dataframe with all checked vars

# Date variables:
# Time relevant int
biddate <- colnames(orig_data)[grepl("^biddate*",colnames(orig_data))]
bidhour <- colnames(orig_data)[grepl("^bidhour*",colnames(orig_data))]
bidminute <- colnames(orig_data)[grepl("^bidminute*",colnames(orig_data))]
bidsecond <- colnames(orig_data)[grepl("^bidsecond*",colnames(orig_data))]
int.time <- c('year',biddate,bidhour,bidminute,bidsecond,'endhour','endingdate','startingdate','endday')
df2 <- cbind(df2,suppressWarnings(sapply(X=orig_data[int.time], FUN=as.integer)))
orig_data <- orig_data[,-which(colnames(orig_data) %in% int.time)] ##update data set unchecked
list <- append(list,int.time)

# Int:
# Post feature Int: 
int <- c('text','itemnum','miles','numbids','address','store','pwrseller',
         'highbidderfdback','reserve','buyitnow','sellfdbackpct','photos',
         'descriptionsize','options','doors','trans','webpage','title',
         'condition','model','cyl','length','age','age2','html','sellerborn',
         'week','auction','n','totallisted','totalsold','carmodel',
         'ageXphoto','warrantyXphoto','warrantyXage','warrantyXageXphotos',
         'group','auc_mins','maxbidtime')

# ----> Some problem df2 <- cbind(df2,suppressWarnings(sapply(X=orig_data[int], FUN=as.integer)))
orig_data <- orig_data[,-which(colnames(orig_data) %in% int)] ##update data set unchecked
list <- append(list,int)

# Numeric
num <- c('bookvalue','pctfdback','startbid','sellerage','negpct','compindex',c(paste0('biddy',1:22)))
lognum <- c('logmiles','logtext','logsize','logstart','logfdback','logphotos',
            'loghtml','logage','logbook',c(paste0('logbid',1:3)))
num.full <- c(num,lognum)
df2 <- cbind(df2,suppressWarnings(sapply(X=orig_data[num.full], FUN=as.numeric)))
orig_data <- orig_data[,-which(colnames(orig_data) %in% num.full)] ##update data set unchecked
list <- append(list,num.full)

# factor
ding <- colnames(orig_data)[grepl("^ding*",colnames(orig_data))] # assign all dummy vars relavant to 'ding'
scratch <- colnames(orig_data)[grepl("^scratch*",colnames(orig_data))]
dent <- colnames(orig_data)[grepl("^dent*",colnames(orig_data))]
broken <- colnames(orig_data)[grepl("^broken*",colnames(orig_data))]
crack <- colnames(orig_data)[grepl("^crack*",colnames(orig_data))]
problem <- colnames(orig_data)[grepl("^problem*",colnames(orig_data))]
rust <- colnames(orig_data)[grepl("^rust*",colnames(orig_data))]

contmode <- colnames(orig_data)[grepl("^contcarmode*",colnames(orig_data))]
contyr <- colnames(orig_data)[grepl("^contyear*",colnames(orig_data))]
contwk <- colnames(orig_data)[grepl("^contweek*",colnames(orig_data))]
con2mode <- colnames(orig_data)[grepl("^con2carmode*",colnames(orig_data))]
con2yr <- colnames(orig_data)[grepl("^con2year*",colnames(orig_data))]
con2n <- colnames(orig_data)[grepl("^con2n*",colnames(orig_data))]
con2wk <- colnames(orig_data)[grepl("^con2week*",colnames(orig_data))]

factor <- c('inspection','relistflag','featured','phone','addedinfo',
            ding,scratch,dent,broken,crack,problem,rust,
            'endsunday','primetime','warranty','relist','sell','dealer',
            contmode,contyr,contwk,con2n)
df2 <- cbind(df2,suppressWarnings(sapply(X=orig_data[factor], FUN=as.factor)))
orig_data <- orig_data[,-which(colnames(orig_data) %in% factor)] ##update data set unchecked
list <- append(list,factor)
orig_data <- cbind(df2,orig_data)
rm(df2)  

# ------------------------------------------------------------------
# Sachin's part
# I have checked the "orig_data" data file and there are no completely empty columns in it.
# But, I found some columns that have a lot of NA values. 
# I have create the "data_na" dataset that contain NA values columns. Check the following dataset.
# biddername15, biddy15, biddername16, biddy16, biddername17, biddy17, biddername18, biddy18, biddername19, biddy19, biddername20, biddy20, biddername21, biddy21, biddername22, biddy22, biddate15, bidhour15, bidminute15, bidsecond15, biddate16, bidhour16, bidminute16, bidsecond16, biddate17, bidhour17, bidminute17, bidsecond17, biddate18, bidhour18, bidminute18, bidsecond18, biddate19, bidhour19, bidminute19, bidsecond19, biddate20, bidhour20, bidminute20, bidsecond20, biddate21, bidhour21, bidminute21, bidsecond21, biddate22, bidhour22, bidminute22, bidsecond22

# data_na <- subset(df2, select=c(biddy15, biddy16, biddy17, biddy18, biddy19, biddy20, 
# biddy21, biddy22, biddate15, bidhour15, bidminute15, bidsecond15, biddate16, bidhour16, bidminute16, bidsecond16,
# biddate17, bidhour17, bidminute17, bidsecond17, biddate18, bidhour18, bidminute18, bidsecond18, biddate19, bidhour19, bidminute19, bidsecond19,
# biddate20, bidhour20, bidminute20, bidsecond20, biddate21, bidhour21, bidminute21, bidsecond21, biddate22, bidhour22, bidminute22, bidsecond22))


# Count the nubmer of NA.
# data_na$na_count <- apply(is.na(data_na), 1, sum)

# Do we need to remove these columns?

# Remove all NA values from the data.
# data_na <- na.omit(data_na)


glimpse(orig_data)

# -------------------------------------------------------------------------
# This original dataset is a mixture of 'raw variables' and 'variables derived by the author'.
# For further analysis, we might need to create three datasets rapidly as we test different models. 
# 1) [both] Dataset with all the variables
# 2) [raw] Dataset with only the raw variables (and not the derived variables created by the author)
# 3) [derived] Dataset with only the derived variables created by the author (and not the raw variables)

# The easiest way to do this is via functions. So, we create a function called 'dataset()' which takes the 
# argument of what dataset type we want, and re-structures the dataset.

# --------------------
# Function description
# --------------------
# For full dataset: "both"
# For only raw vars: "raw"
# For only derived vars: "derived"

dataset <- function(type){
  derived <- c("ding_good", "ding_bad", "ding_knowledge", "ding_negation", "ding_low", "ding_high", "ding_pics", 
               "dent_good", "dent_bad", "dent_knowledge", "dent_negation", "dent_low", "dent_high",  "rust_high",
               "dent_pics", "crack_good", "crack_bad", "crack_knowledge", "crack_negation", "crack_low", "crack_high",
               "crack_pics", "problem_good", "problem_bad", "problem_knowledge", "problem_negation", "problem_low",
               "problem_high","problem_pics", "rust_good", "rust_bad", "rust_knowledge", "rust_negation", "rust_low",
               "rust_pics", "scratch_good", "scratch_bad", "scratch_knowledge", "scratch_negation", "scratch_low",
               "scratch_high", "scratch_pics", "broken_good", "broken_bad", "broken_knowledge", "broken_negation",
               "broken_low", "broken_high", "broken_pics", "ding_group", "scratch_group", "crack_group", "broken_group",
               "dent_group", "problem_group", "rust_group")
  
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
  if (type == "both") {
    data <<- orig_data
  }
  else if(type == "raw"){
    data <<- orig_data[ , !(names(orig_data) %in% derived)]
  }
  else if(type == "derived"){
    data <<- orig_data[ , !(names(orig_data) %in% raw)]
  }
}

# Function complete: Use 'data' for the dataset -----------------------------



# Creating a data set with all the vars
dataset("raw")
=======

df1 <- read_dta("./112439-V1/temp for project.dta")
# orig_data <- data.frame(df1)

dim(orig_data)
# We have 146734 rows and 605 variables. 

# Remove obviously unnecessary columns (PW)
# -----------------------------------------
orig_data = orig_data[ , -which(colnames(orig_data) %in% c("membersince","name","vin","highbiddername",
                                                           "sellername","questions","X_merge","temp","biddername1","biddername2",
                                                           "biddername3", "biddername4", "biddername5","biddername6","biddername7",
                                                           "biddername8","biddername9","biddername10","biddername11","biddername12",
                                                           "biddername13","biddername14","biddername15","biddername16","biddername17",
                                                           "biddername18","biddername19","biddername20","biddername21","biddername22"))]

# We have 146734 rows and 575 variables (We removed 30 variables).

# Fixing date and time date and time (JL)
# --------------------------------------
## generally, two types of datetime vars here: standard and numeric
## turn standard to time var:

orig_data$enddate = strptime(orig_data$enddate, "%b-%d-%y %H:%M:%S")
orig_data$startdate = strptime(orig_data$startdate, 
                               "%b-%d-%y %H:%M:%S")
### auction last in mins
orig_data$auc_mins = as.numeric(difftime(orig_data$enddate,
                                         orig_data$startdate,
                                         units="mins"))

### The var "length" is auction lasts in days does the same thing


### note that any var that link "bid" and "1" (eg. logbid1) relates to
### the highest bid

## deal with nemeric time var:
### to calculate how long it takes for the highest bidder to make 
### decision (in days)

orig_data$maxbidtime = orig_data$biddate1 - orig_data$startingdate

### I think bidhour, bidminute, bidsec, etc should not be necessary
### the best way to handle time is to calculate duration
### i.e. time difference
### it doesn't make sense to include a numeric value i.e. bidhour


# end of JL's part
# --------------------

# ------------------------------------------------------------------
# Sachin's part
# I have checked the "orig_data" data file and there are no completely empty columns in it.
# But, I found some columns that have a lot of NA values. 
# I have create the "data_na" dataset that contain NA values columns. Check the following dataset.
# biddername15, biddy15, biddername16, biddy16, biddername17, biddy17, biddername18, biddy18, biddername19, biddy19, biddername20, biddy20, biddername21, biddy21, biddername22, biddy22, biddate15, bidhour15, bidminute15, bidsecond15, biddate16, bidhour16, bidminute16, bidsecond16, biddate17, bidhour17, bidminute17, bidsecond17, biddate18, bidhour18, bidminute18, bidsecond18, biddate19, bidhour19, bidminute19, bidsecond19, biddate20, bidhour20, bidminute20, bidsecond20, biddate21, bidhour21, bidminute21, bidsecond21, biddate22, bidhour22, bidminute22, bidsecond22

data_na <- subset(orig_data, select=c(biddy15, biddy16, biddy17, biddy18, biddy19, biddy20, 
                                      biddy21, biddy22, biddate15, bidhour15, bidminute15, bidsecond15, biddate16, bidhour16, bidminute16, bidsecond16, 
                                      biddate17, bidhour17, bidminute17, bidsecond17, biddate18, bidhour18, bidminute18, bidsecond18, biddate19, bidhour19, bidminute19, bidsecond19, 
                                      biddate20, bidhour20, bidminute20, bidsecond20, biddate21, bidhour21, bidminute21, bidsecond21, biddate22, bidhour22, bidminute22, bidsecond22))


# Count the nubmer of NA.
data_na$na_count <- apply(is.na(data_na), 1, sum)

# Do we need to remove these columns?

# Remove all NA values from the data.
#data_na <- na.omit(data_na)


# Fixing date and time
# --------------------------------------------------------------
# 1. Use the "enddate", "startdate" to calculate month indicator (1,...,12)

orig_data$start_m = match(substr(orig_data$startdate, 1, 3), month.abb)
orig_data$end_m = match(substr(orig_data$enddate, 1, 3), month.abb)

# 2. Use month indicator to calculate season

attach(orig_data)
orig_data$start_s <- ifelse(start_m==12 | start_m==1 | start_m==2, "Winter", 
                            ifelse(start_m==3 | start_m==4 | start_m==5, "Spring", 
                                   ifelse(start_m==6 | start_m==7 | start_m==8, "Summer",
                                          ifelse(start_m==9 | start_m==10 | start_m==11, "Fall", 
                                                 NA))))

orig_data$end_s <- ifelse(end_m==12 | end_m==1 | end_m==2, "Winter", 
                            ifelse(end_m==3 | end_m==4 | end_m==5, "Spring", 
                                   ifelse(end_m==6 | end_m==7 | end_m==8, "Summer",
                                          ifelse(end_m==9 | end_m==10 | end_m==11, "Fall", 
                                                 NA))))

# 2a. Create dummy indicating if auction happened in car-demand peak season
orig_data$peak_s = ifelse(orig_data$start_s=="Spring" | orig_data$start_s=="Fall",
                          1, 0)

# 2b. Create dummy indicating if start and end season are different
orig_data$season_trans = ifelse(orig_data$start_s!=orig_data$end_s,
                                1, 0)

# 3. Var "length" is auction lasts in days, all set
# 4. Var "endday" is weekday indicator (1,...,7), all set
# 5. Var "endhour" is which the hour (24hr-clock) the auction ends, all set
# 6. Var "endsunday" is a dummy pointing if endday is Sunday, all set

# 7. Among "biddate1" "bidhour1" "bidminute1" "bidsecond1":
## 7a. Calculate how long it took for the highest bid to appear
orig_data$max_evot = orig_data$biddate1 - orig_data$startingdate

# 7Note: All others from "biddate2" to "bidsecond22"...are no longer useful here 


# -------------------------------------------------------------------------
# This original dataset is a mixture of 'raw variables' and 'variables derived by the author'.
# For further analysis, we might need to create three datasets rapidly as we test different models. 
# 1) [both] Dataset with all the variables
# 2) [raw] Dataset with only the raw variables (and not the derived variables created by the author)
# 3) [derived] Dataset with only the derived variables created by the author (and not the raw variables)

# The easiest way to do this is via functions. So, we create a function called 'dataset()' which takes the 
# argument of what dataset type we want, and re-structures the dataset.

# --------------------
# Function description
# --------------------
# For full dataset: "both"
# For only raw vars: "raw"
# For only derived vars: "derived"

dataset <- function(type){
  derived <- c("ding_good", "ding_bad", "ding_knowledge", "ding_negation", "ding_low", "ding_high", "ding_pics", 
               "dent_good", "dent_bad", "dent_knowledge", "dent_negation", "dent_low", "dent_high",  "rust_high",
               "dent_pics", "crack_good", "crack_bad", "crack_knowledge", "crack_negation", "crack_low", "crack_high",
               "crack_pics", "problem_good", "problem_bad", "problem_knowledge", "problem_negation", "problem_low",
               "problem_high","problem_pics", "rust_good", "rust_bad", "rust_knowledge", "rust_negation", "rust_low",
               "rust_pics", "scratch_good", "scratch_bad", "scratch_knowledge", "scratch_negation", "scratch_low",
               "scratch_high", "scratch_pics", "broken_good", "broken_bad", "broken_knowledge", "broken_negation",
               "broken_low", "broken_high", "broken_pics", "ding_group", "scratch_group", "crack_group", "broken_group",
               "dent_group", "problem_group", "rust_group")
  
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
  if (type == "both") {
    data <<- orig_data
  }
  else if(type == "raw"){
    data <<- orig_data[ , !(names(orig_data) %in% derived)]
  }
  else if(type == "derived"){
    data <<- orig_data[ , !(names(orig_data) %in% raw)]
  }
}

# Function complete: Use 'data' for the dataset -----------------------------



# Creating a data set with all the vars
dataset("raw")



# ---------------Startup--------------------
# View all types
glimpse(orig_data)

## Create a full list of var names 
all <- colnames(orig_data)
all
## A list of var names with good types
list <- c()

# -------------
# Working on character variables
list <- append(list,'sellerid')
df2 <- orig_data['sellerid']

orig_data <- orig_data[,-which(colnames(orig_data) %in% list)] ##update data set unchecked

# Date variables:
# Time relevant int
biddate <- colnames(orig_data)[grepl("^biddate*",colnames(orig_data))]
bidhour <- colnames(orig_data)[grepl("^bidhour*",colnames(orig_data))]
bidminute <- colnames(orig_data)[grepl("^bidminute*",colnames(orig_data))]
bidsecond <- colnames(orig_data)[grepl("^bidsecond*",colnames(orig_data))]
int.time <- c('year',biddate,bidhour,bidminute,bidsecond,'endhour','endingdate','startingdate','endday')
df2 <- cbind(df2,suppressWarnings(lapply(X=orig_data[int.time], FUN=as.integer)))
orig_data <- orig_data[,-which(colnames(orig_data) %in% int.time)] ##update data set unchecked
list <- append(list,int.time)

# Int:
# Post feature Int: 
int <- c('text','itemnum','miles','numbids','address','store','pwrseller',
         'highbidderfdback','reserve','buyitnow','sellfdbackpct','photos',
         'descriptionsize','options','doors','trans','webpage','title',
         'condition','model','cyl','length','age','age2','html','sellerborn',
         'week','auction','n','totallisted','totalsold','carmodel',
         'ageXphoto','warrantyXphoto','warrantyXage','warrantyXageXphotos',
         'group','auc_mins','maxbidtime','auc_duration_mins','max_evot')

cbind(df2,suppressWarnings(lapply(X=orig_data[int], FUN=as.integer)))
orig_data <- orig_data[,-which(colnames(orig_data) %in% int)] ##update data set unchecked
list <- append(list,int)

# Numeric
num <- c('bookvalue','pctfdback','startbid','sellerage','negpct','compindex',c(paste0('biddy',1:22)))
lognum <- c('logmiles','logtext','logsize','logstart','logfdback','logphotos',
            'loghtml','logage','logbook',c(paste0('logbid',1:3)))
num.full <- c(num,lognum)
df2 <- cbind(df2,suppressWarnings(lapply(X=orig_data[num.full], FUN=as.numeric)))
orig_data <- orig_data[,-which(colnames(orig_data) %in% num.full)] ##update data set unchecked
list <- append(list,num.full)

# factor
ding <- colnames(orig_data)[grepl("^ding*",colnames(orig_data))] # assign all dummy vars relavant to 'ding'
scratch <- colnames(orig_data)[grepl("^scratch*",colnames(orig_data))]
dent <- colnames(orig_data)[grepl("^dent*",colnames(orig_data))]
broken <- colnames(orig_data)[grepl("^broken*",colnames(orig_data))]
crack <- colnames(orig_data)[grepl("^crack*",colnames(orig_data))]
problem <- colnames(orig_data)[grepl("^problem*",colnames(orig_data))]
rust <- colnames(orig_data)[grepl("^rust*",colnames(orig_data))]

contmode <- colnames(orig_data)[grepl("^contcarmode*",colnames(orig_data))]
contyr <- colnames(orig_data)[grepl("^contyear*",colnames(orig_data))]
contwk <- colnames(orig_data)[grepl("^contweek*",colnames(orig_data))]
con2mode <- colnames(orig_data)[grepl("^con2carmode*",colnames(orig_data))]
con2yr <- colnames(orig_data)[grepl("^con2year*",colnames(orig_data))]
con2n <- colnames(orig_data)[grepl("^con2n*",colnames(orig_data))]
con2wk <- colnames(orig_data)[grepl("^con2week*",colnames(orig_data))]

factor <- c('inspection','relistflag','featured','phone','addedinfo',
            ding,scratch,dent,broken,crack,problem,rust,
            'endsunday','primetime','warranty','relist','sell','dealer',
            'maker','interior','exterior','location','software','caradphotos',
            contmode,contyr,contwk,con2n)
df2 <- cbind(df2,lapply(X=orig_data[factor], FUN=as.factor))
orig_data <- orig_data[,-which(colnames(orig_data) %in% factor)] ##update data set unchecked
list <- append(list,factor)

orig_data <- cbind(df2,orig_data)
rm(df2)  


