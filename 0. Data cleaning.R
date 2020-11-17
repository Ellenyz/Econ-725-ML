# This is the script that we will use for data cleaning. 
# ----------------
# Instructions
# ----------------
# 1. Please navigate to the 'datacleaning' branch when making changes to this file. 
# 2. Please commit every small change and give a clear description of the change in the commit message
# 3. Please comment on the code as clearly as possible
# --------------------------
rm(list=ls())
library(haven)
library(tidyverse) #includes the dplyr and tidyr packages
library(dplyr)
library(tidyr)

#setwd('C:/Users/Sachin/Desktop/my_725_project_work')
# Importing the dataset

df1 <- read_dta("temp.dta")
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



# Setting correct variable types
# -------------------------------
## Create a full list of var names for debug
all <- colnames(orig_data)
## A list of var names with good types for debug
list <- c()

# -------------
## character
orig_data$itemnum <- as.factor(orig_data$itemnum)
list <- c('itemnum')
all <- all[which(!all %in% list)]

## factor
condition <- c("^ding*","^scratch*","^dent*","^broken*","^crack*","^problem*","^rust*")  #list all condition relevant vars
fac_list <- c()
for (i in condition){
  vec <- colnames(orig_data)[grepl(i,colnames(orig_data))]
  fac_list <- append(fac_list,vec)
}

fac_list <- c(fac_list,'inspection','relistflag','featured','phone','addedinfo',
              'endsunday','primetime','warranty','relist','sell','dealer',
              'maker','interior','exterior','location','software','caradphotos')  #list all factors
orig_data <- orig_data %>% mutate_at(.vars = fac_list, .funs = as.factor)
list <- append(list,fac_list)
all <- all[which(!all %in% list)]

## Integers
time <- c("^biddate*","^bidhour*","^bidminute*","^bidsecond*")
int_list <- c()
for (i in time){
  vec <- colnames(orig_data)[grepl(i,colnames(orig_data))]
  int_list <- append(int_list,vec)
}
int_list <- c(int_list,'year','endhour','endingdate','startingdate','endday','text',
              'miles','numbids','address','store','pwrseller','highbidderfdback',
              'reserve','buyitnow','sellfdbackpct','photos','descriptionsize','options','doors',
              'trans','webpage','title','condition','model','cyl','length','age','age2','html',
              'sellerborn','week','auction','n','totallisted','totalsold','maxbidtime',
              'auc_duration_mins','max_evot')
orig_data <- orig_data %>% mutate_at(.vars = int_list, .funs = as.integer)
list <- append(list,int_list)
all <- all[which(!all %in% list)]


# Numeric
num_list <- c('bookvalue','pctfdback','startbid','sellerage','negpct','compindex',c(paste0('biddy',1:22)),
              'logmiles','logtext','logsize','logstart','logfdback','logphotos',
              'loghtml','logage','logbook',c(paste0('logbid',1:3)))
orig_data <- orig_data %>% mutate_at(.vars = num_list, .funs = as.numeric)
list <- append(list,num_list)
all <- all[which(!all %in% list)]

glimpse(orig_data[all])  ## Remaining vars are all in good type

rm(all,i,list,num_list,time,vec, condition, fac_list, int_list) ##Remove useless values
dim(orig_data)
glimpse(orig_data)
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
    data <- orig_data
  }
  else if(type == "raw"){
    data <- orig_data[ , !(names(orig_data) %in% derived)]
  }
  else if(type == "derived"){
    data <- orig_data[ , !(names(orig_data) %in% raw)]
  }
}

# Function complete: Use 'data' for the dataset -----------------------------


# Creating a data set with all the vars
dataset("raw")

dim(orig_data)
# We have 146734 rows and 605 variables. 

# ------------------------------------------------------------------------------
#
# Testing Lasso and Ridge 
#
# data_ebay <- subset(orig_data, select=c(logbid1, logmiles, photos, text,
                                   # contcarmode_2, contcarmode_3, contcarmode_4, contcarmode_5, contcarmode_6, contcarmode_7, contcarmode_8, contcarmode_9, contcarmode_10, contcarmode_11,
                                   # contcarmode_12, contcarmode_13, contcarmode_15, contcarmode_16, contcarmode_17, contcarmode_18, contcarmode_19, contcarmode_20, contcarmode_21, contcarmode_22, contcarmode_23, contcarmode_24,
                                   # contcarmode_25, contcarmode_26, contcarmode_27, contcarmode_28, contcarmode_29, contcarmode_30, contcarmode_31, contcarmode_32, contcarmode_33, contcarmode_34, contcarmode_35, contcarmode_36,
                                   # contcarmode_37, contcarmode_38, contcarmode_39, contcarmode_40, contcarmode_41, contcarmode_42, contcarmode_43, contcarmode_44, contcarmode_45, contcarmode_46, contcarmode_47, contcarmode_48,
                                   # contcarmode_49, contcarmode_51, contcarmode_52, contcarmode_53, contcarmode_54, contcarmode_55, contcarmode_56, contcarmode_57, contcarmode_58, contcarmode_59, contcarmode_60, contcarmode_61, contcarmode_62,
                                   # contyear_1951,contyear_1952,contyear_1953,contyear_1954,contyear_1955,contyear_1956,contyear_1957,contyear_1958,contyear_1959,contyear_1960,contyear_1961,contyear_1962,contyear_1963,contyear_1964,contyear_1965,
                                   # contyear_1966,contyear_1967,contyear_1968,contyear_1969,contyear_1970,contyear_1971,contyear_1972,contyear_1973,contyear_1974,contyear_1975,contyear_1976,contyear_1977,contyear_1978,contyear_1979,contyear_1980,
                                   # contyear_1981,contyear_1982,contyear_1983,contyear_1984,contyear_1985,contyear_1986,contyear_1987,contyear_1988,contyear_1989,contyear_1990,contyear_1991,contyear_1992,contyear_1993,contyear_1994,contyear_1995,
                                   # contyear_1996,contyear_1997,contyear_1998,contyear_1999,contyear_2000,contyear_2001,contyear_2002,contyear_2003,contyear_2004,contyear_2005,contyear_2006,contyear_2007,
                                   # contweek_2, contweek_3, contweek_4, contweek_5, contweek_6, contweek_7,contweek_8, contweek_9, contweek_10, contweek_11, contweek_12, contweek_13, contweek_14,contweek_15,contweek_16, contweek_17,
                                   # contweek_18, contweek_20, contweek_21, contweek_22, contweek_23, contweek_24, contweek_25,contweek_26, contweek_27, contweek_28, contweek_29, contweek_30, contweek_31, contweek_32, contweek_33, contweek_34, contweek_35))


# maker, text, sell, photos, options, biddy1, biddy2, biddy3, biddy4, biddy5, biddy6, biddy7, biddy8, biddy10, biddy11, biddy12, biddy13, biddy14, biddy15, biddy16, biddy17, biddy18, biddy19, biddy20, biddy21, biddy22,
#write.csv(orig_data, "orig_data.csv")

# Keep only sell cars
orig_data <- orig_data[orig_data$sell ==1, ]
#write.csv(orig_data, "orig_data.csv")

lk <- lm(logbid1~orig_data$maker,data = orig_data)
summary(lk)

data_ebay <- subset(orig_data, select = c(logbid1, logmiles, logtext, ding, ding_noticeable, ding_many, ding_barely, ding_large, model, cyl, warranty, age, maker,
                                          ding_obvious, ding_major, ding_small, ding_one, ding_almost, ding_photo, ding_pic, ding_some, ding_minor, ding_visible, ding_little, ding_no, ding_two, ding_nothing, ding_tiny,
                                          ding_only, ding_several, ding_few, ding_hardly, ding_never, ding_very, ding_couple, ding_much, ding_big, ding_free, ding_significant, ding_extremely, ding_good, ding_bad, ding_knowledge, ding_negation, ding_low, ding_high, ding_pics, ding_group,
                                          scratch, scratch_noticeable, scratch_many, scratch_barely, scratch_large, scratch_major, scratch_small, scratch_one, scratch_almost, scratch_photo, scratch_pic, scratch_some, scratch_minor, scratch_visible, scratch_little, scratch_no, scratch_two, scratch_nothing, scratch_tiny, scratch_only, scratch_several, scratch_few, 
                                          scratch_hardly, scratch_never, scratch_very, scratch_couple, scratch_much, scratch_big, scratch_free, scratch_significant, scratch_extremely,scratch_good, scratch_bad, scratch_negation, scratch_low, scratch_high, scratch_pics, scratch_group,
                                          crack, crack_noticeable, crack_many, crack_barely, crack_large, crack_major, crack_small, crack_one, crack_almost, crack_photo, crack_pic, crack_some, crack_minor, crack_visible, crack_little, crack_no, crack_two, crack_nothing, crack_tiny, 
                                          crack_only, crack_several, crack_few, crack_hardly, crack_never, crack_very, crack_couple, crack_much, crack_big, crack_free, crack_good, crack_bad, crack_knowledge, crack_negation, crack_low, crack_high, crack_pics, crack_group,
                                          dent, dent_noticeable, dent_many, dent_barely, dent_large, dent_major, dent_small, dent_one, dent_almost, dent_photo, dent_pic, dent_some, dent_minor, dent_visible, dent_huge, dent_little, dent_no, dent_two, dent_nothing, dent_tiny, dent_only, 
                                          dent_several, dent_few, dent_hardly, dent_medium, dent_never, dent_very, dent_couple, dent_much, dent_big, dent_free, dent_significant, dent_extremely, dent_invisible, dent_good, dent_bad, dent_knowledge, dent_negation, dent_high, dent_low, dent_pics, dent_group,
                                          broken, broken_noticeable, broken_many, broken_barely, broken_large, broken_major, broken_small, broken_almost, broken_one, broken_photo, broken_pic, broken_some, broken_minor, broken_visible, broken_little, broken_no, broken_two, broken_nothing, broken_tiny, broken_only, 
                                          broken_several, broken_few, broken_hardly, broken_known, broken_never, broken_very, broken_apparent, broken_couple, broken_much, broken_big, broken_free, broken_extremely, broken_good, broken_bad, broken_knowledge, broken_negation, broken_low, broken_high, broken_pics, broken_group,
                                          rust, rust_wide, rust_noticeable, rust_many, rust_barely, rust_minute, rust_large, rust_obvious, rust_major, rust_small, rust_one, rust_almost, rust_photo, rust_pic, rust_some, rust_limited, rust_minor, rust_substantial, rust_rarely, rust_visible, rust_huge, rust_little, rust_no, rust_two, 
                                          rust_nothing, rust_tiny, rust_only, rust_several, rust_few, rust_hardly, rust_medium, rust_known, rust_never, rust_very, rust_apparent, rust_couple, rust_much, rust_big, rust_free, rust_significant, rust_extremely, rust_good, rust_bad, rust_knowledge, rust_negation, rust_low, rust_high, rust_pics, rust_group,
                                          problem, problem_noticeable, problem_many, problem_barely, problem_minute, problem_large, problem_obvious, problem_major, problem_small, problem_one, problem_almost, problem_photo, problem_pic, problem_some, problem_limited, problem_minor,problem_rarely, problem_visible, problem_little, problem_no, problem_two, 
                                          problem_nothing, problem_tiny, problem_only, problem_several, problem_few, problem_hardly, problem_medium, problem_known, problem_never, problem_very, problem_apparent, problem_couple, problem_much, problem_big, problem_free, problem_significant, problem_extremely, problem_vast, problem_good, problem_bad, problem_knowledge, problem_negation, problem_low, problem_high, problem_pics, problem_group,
                                          carmodel, contcarmode_2, contcarmode_3, contcarmode_4, contcarmode_5, contcarmode_6, contcarmode_7, contcarmode_8, contcarmode_9, contcarmode_10, contcarmode_11,
                                          contcarmode_12, contcarmode_13, contcarmode_15, contcarmode_16, contcarmode_17, contcarmode_18, contcarmode_19, contcarmode_20, contcarmode_21, contcarmode_22, contcarmode_23, contcarmode_24,
                                          contcarmode_25, contcarmode_26, contcarmode_27, contcarmode_28, contcarmode_29, contcarmode_30, contcarmode_31, contcarmode_32, contcarmode_33, contcarmode_34, contcarmode_35, contcarmode_36,
                                          contcarmode_37, contcarmode_38, contcarmode_39, contcarmode_40, contcarmode_41, contcarmode_42, contcarmode_43, contcarmode_44, contcarmode_45, contcarmode_46, contcarmode_47, contcarmode_48,
                                          contcarmode_49, contcarmode_51, contcarmode_52, contcarmode_53, contcarmode_54, contcarmode_55, contcarmode_56, contcarmode_57, contcarmode_58, contcarmode_59, contcarmode_60, contcarmode_61,
                                          contyear_1951,contyear_1952,contyear_1953,contyear_1954,contyear_1955,contyear_1956,contyear_1957,contyear_1958,contyear_1959,contyear_1960,contyear_1961,contyear_1962,contyear_1963,contyear_1964,contyear_1965,
                                          contyear_1966,contyear_1967,contyear_1968,contyear_1969,contyear_1970,contyear_1971,contyear_1972,contyear_1973,contyear_1974,contyear_1975,contyear_1976,contyear_1977,contyear_1978,contyear_1979,contyear_1980,
                                          contyear_1981,contyear_1982,contyear_1983,contyear_1984,contyear_1985,contyear_1986,contyear_1987,contyear_1988,contyear_1989,contyear_1990,contyear_1991,contyear_1992,contyear_1993,contyear_1994,contyear_1995,
                                          contyear_1996,contyear_1997,contyear_1998,contyear_1999,contyear_2000,contyear_2001,contyear_2002,contyear_2003,contyear_2004,contyear_2005,contyear_2006,contyear_2007,
                                          contweek_2, contweek_3, contweek_4, contweek_5, contweek_6, contweek_7,contweek_8, contweek_9, contweek_10, contweek_11, contweek_12, contweek_13, contweek_14,contweek_15,contweek_16, contweek_17,
                                          contweek_18, contweek_20, contweek_21, contweek_22, contweek_23, contweek_24, contweek_25,contweek_26, contweek_27, contweek_28, contweek_29, contweek_30, contweek_31, contweek_32, contweek_33, contweek_34, contweek_35,
                                          con2carmode_2, con2carmode_3, con2carmode_4, con2carmode_5, con2carmode_6, con2carmode_7, con2carmode_8, con2carmode_9, con2carmode_10, con2carmode_11, con2carmode_12, con2carmode_13, con2carmode_15, con2carmode_16, con2carmode_17,
                                          con2carmode_18, con2carmode_19, con2carmode_20, con2carmode_21, con2carmode_22, con2carmode_23, con2carmode_24, con2carmode_25, con2carmode_26, con2carmode_27, con2carmode_28, con2carmode_29, con2carmode_30, con2carmode_31, 
                                          con2carmode_32, con2carmode_33, con2carmode_34, con2carmode_35, con2carmode_36, con2carmode_37, con2carmode_38, con2carmode_39, con2carmode_40, con2carmode_41, con2carmode_42, con2carmode_43, con2carmode_44, con2carmode_45, 
                                          con2carmode_46, con2carmode_47, con2carmode_48, con2carmode_49, con2carmode_51, con2carmode_52, con2carmode_53, con2carmode_54, con2carmode_55, con2carmode_56, con2carmode_57, con2carmode_58, con2carmode_59, con2carmode_60, con2carmode_61,
                                          con2year_1951, con2year_1952, con2year_1953, con2year_1954, con2year_1955, con2year_1956, con2year_1957, con2year_1958, con2year_1959, con2year_1960, con2year_1961, con2year_1962, con2year_1963, con2year_1964, con2year_1965, con2year_1966, 
                                          con2year_1967, con2year_1968, con2year_1969, con2year_1970, con2year_1971, con2year_1972, con2year_1973, con2year_1974, con2year_1975, con2year_1976, con2year_1977, con2year_1978, con2year_1979, con2year_1980, con2year_1981, con2year_1982, 
                                          con2year_1983, con2year_1984, con2year_1985, con2year_1986, con2year_1987, con2year_1988, con2year_1989, con2year_1990, con2year_1991, con2year_1992, con2year_1993, con2year_1994, con2year_1995, con2year_1996, con2year_1997, con2year_1998, 
                                          con2year_1999, con2year_2000, con2year_2001, con2year_2002, con2year_2003, con2year_2004, con2year_2005, con2year_2006, con2year_2007, con2n_1, con2n_2, con2n_3, con2n_4, con2n_5, con2n_6, con2n_7, con2n_8, con2n_9, con2n_10, con2n_11, con2n_12, 
                                          con2n_13, con2n_14, con2n_15, con2n_16, con2n_17, con2n_18, con2n_19, con2n_20, con2n_21, con2n_22, con2week_2, con2week_3, con2week_4, con2week_5, con2week_6, con2week_7, con2week_8, con2week_9, con2week_10, con2week_11, con2week_12, con2week_13, 
                                          con2week_14, con2week_15, con2week_16, con2week_17, con2week_18, con2week_20, con2week_21, con2week_22, con2week_23, con2week_24, con2week_25, con2week_26, con2week_27, con2week_28, con2week_29, con2week_30, con2week_31, con2week_32, con2week_33, 
                                          con2week_34, con2week_35))


# Remove NA Values
data_ebay <- data_ebay[!(is.na(data_ebay$logbid1)),]
#data_ebay <- data_ebay[!(is.na(data_ebay$photos)),]
#data_ebay <- data_ebay[!(is.na(data_ebay$bookvalue)),]

#data_ebay <- data_ebay[(data_ebay$biddy<100000),]
min(data_ebay$logbid1)
max(data_ebay$logbid1)

#data_ebay <- data_ebay[data_ebay$sell ==1, ]
#data_ebay <- subset(data_ebay, select = c(logbid1))
#----------------------------------------------------------------------------------
#
set.seed(1)

rn <- sample(seq_len(nrow(data_ebay)),size = 1000)  
test <- data_ebay[rn,] 
train <- data_ebay[-rn,] 

# linear probability model
linear_pro_model = lm(logbid1~.,data=train)
summary(linear_pro_model)

# Linear MSE
mse1 <- mean((test$"logbid1" - predict(linear_pro_model,test))^2)
mse1
#---------------------------------------------------------------------------------
#
set.seed(1)
train <- sample(1:nrow(data_ebay), size=round(0.98*nrow(data_ebay)), replace=FALSE)
test = -train

# # Linear model
# lm.fit <- lm(logbid1 ~ ., data = data_ebay, subset = train )   
# attach (data_ebay)
# mse1 <- mean((logbid1-predict(lm.fit ,data_ebay))[test]^2)
# mse1

# Predication
# data_ebay$linear_pro_model_pred = predict(lm.fit ,data_ebay)

# Using Ploy function
covariates <- subset(data_ebay, select=c(logmiles, logtext, ding, ding_noticeable, ding_many, ding_barely, ding_large, model, cyl, warranty, age,
                                         ding_obvious, ding_major, ding_small, ding_one, ding_almost, ding_photo, ding_pic, ding_some, ding_minor, ding_visible, ding_little, ding_no, ding_two, ding_nothing, ding_tiny, maker,
                                         ding_only, ding_several, ding_few, ding_hardly, ding_never, ding_very, ding_couple, ding_much, ding_big, ding_free, ding_significant, ding_extremely, ding_good, ding_bad, ding_knowledge, ding_negation, ding_low, ding_high, ding_pics, ding_group,
                                         scratch, scratch_noticeable, scratch_many, scratch_barely, scratch_large, scratch_major, scratch_small, scratch_one, scratch_almost, scratch_photo, scratch_pic, scratch_some, scratch_minor, scratch_visible, scratch_little, scratch_no, scratch_two, scratch_nothing, scratch_tiny, scratch_only, scratch_several, scratch_few,
                                         scratch_hardly, scratch_never, scratch_very, scratch_couple, scratch_much, scratch_big, scratch_free, scratch_significant, scratch_extremely,scratch_good, scratch_bad, scratch_negation, scratch_low, scratch_high, scratch_pics, scratch_group,
                                         crack, crack_noticeable, crack_many, crack_barely, crack_large, crack_major, crack_small, crack_one, crack_almost, crack_photo, crack_pic, crack_some, crack_minor, crack_visible, crack_little, crack_no, crack_two, crack_nothing, crack_tiny, 
                                         crack_only, crack_several, crack_few, crack_hardly, crack_never, crack_very, crack_couple, crack_much, crack_big, crack_free, crack_good, crack_bad, crack_knowledge, crack_negation, crack_low, crack_high, crack_pics, crack_group, 
                                         dent, dent_noticeable, dent_many, dent_barely, dent_large, dent_major, dent_small, dent_one, dent_almost, dent_photo, dent_pic, dent_some, dent_minor, dent_visible, dent_huge, dent_little, dent_no, dent_two, dent_nothing, dent_tiny, dent_only, dent_several, 
                                         dent_few, dent_hardly, dent_medium, dent_never, dent_very, dent_couple, dent_much, dent_big, dent_free, dent_significant, dent_extremely, dent_invisible, dent_good, dent_bad, dent_knowledge, dent_negation, dent_high, dent_low, dent_pics, dent_group,
                                         broken, broken_noticeable, broken_many, broken_barely, broken_large, broken_major, broken_small, broken_almost, broken_one, broken_photo, broken_pic, broken_some, broken_minor, broken_visible, broken_little, broken_no, broken_two, broken_nothing, broken_tiny,
                                         broken_only, broken_several, broken_few, broken_hardly, broken_known, broken_never, broken_very, broken_apparent, broken_couple, broken_much, broken_big, broken_free, broken_extremely, broken_good, broken_bad, broken_knowledge, broken_negation, broken_low, broken_high, broken_pics, broken_group,
                                         rust, rust_wide, rust_noticeable, rust_many, rust_barely, rust_minute, rust_large, rust_obvious, rust_major, rust_small, rust_one, rust_almost, rust_photo, rust_pic, rust_some, rust_limited, rust_minor, rust_substantial, rust_rarely, rust_visible, rust_huge, rust_little, rust_no, rust_two, rust_nothing,
                                         rust_tiny, rust_only, rust_several, rust_few, rust_hardly, rust_medium, rust_known, rust_never, rust_very, rust_apparent, rust_couple, rust_much, rust_big, rust_free, rust_significant, rust_extremely, rust_good, rust_bad, rust_knowledge, rust_negation, rust_low, rust_high, rust_pics, rust_group,
                                         problem, problem_noticeable, problem_many, problem_barely, problem_minute, problem_large, problem_obvious, problem_major, problem_small, problem_one, problem_almost, problem_photo, problem_pic, problem_some, problem_limited, problem_minor,problem_rarely, problem_visible, problem_little, problem_no, 
                                         problem_two, problem_nothing, problem_tiny, problem_only, problem_several, problem_few, problem_hardly, problem_medium, problem_known, problem_never, problem_very, problem_apparent, problem_couple, problem_much, problem_big, problem_free, problem_significant, problem_extremely, problem_vast, problem_good, problem_bad, problem_knowledge, problem_negation, problem_low, problem_high, problem_pics, problem_group,
                                         carmodel ,contcarmode_2, contcarmode_3, contcarmode_4, contcarmode_5, contcarmode_6, contcarmode_7, contcarmode_8, contcarmode_9, contcarmode_10, contcarmode_11,
                                         contcarmode_12, contcarmode_13, contcarmode_15, contcarmode_16, contcarmode_17, contcarmode_18, contcarmode_19, contcarmode_20, contcarmode_21, contcarmode_22, contcarmode_23, contcarmode_24,
                                         contcarmode_25, contcarmode_26, contcarmode_27, contcarmode_28, contcarmode_29, contcarmode_30, contcarmode_31, contcarmode_32, contcarmode_33, contcarmode_34, contcarmode_35, contcarmode_36,
                                         contcarmode_37, contcarmode_38, contcarmode_39, contcarmode_40, contcarmode_41, contcarmode_42, contcarmode_43, contcarmode_44, contcarmode_45, contcarmode_46, contcarmode_47, contcarmode_48,
                                         contcarmode_49, contcarmode_51, contcarmode_52, contcarmode_53, contcarmode_54, contcarmode_55, contcarmode_56, contcarmode_57, contcarmode_58, contcarmode_59, contcarmode_60, contcarmode_61,
                                         contyear_1951,contyear_1952,contyear_1953,contyear_1954,contyear_1955,contyear_1956,contyear_1957,contyear_1958,contyear_1959,contyear_1960,contyear_1961,contyear_1962,contyear_1963,contyear_1964,contyear_1965,
                                         contyear_1966,contyear_1967,contyear_1968,contyear_1969,contyear_1970,contyear_1971,contyear_1972,contyear_1973,contyear_1974,contyear_1975,contyear_1976,contyear_1977,contyear_1978,contyear_1979,contyear_1980,
                                         contyear_1981,contyear_1982,contyear_1983,contyear_1984,contyear_1985,contyear_1986,contyear_1987,contyear_1988,contyear_1989,contyear_1990,contyear_1991,contyear_1992,contyear_1993,contyear_1994,contyear_1995,
                                         contyear_1996,contyear_1997,contyear_1998,contyear_1999,contyear_2000,contyear_2001,contyear_2002,contyear_2003,contyear_2004,contyear_2005,contyear_2006,contyear_2007,
                                         contweek_2, contweek_3, contweek_4, contweek_5, contweek_6, contweek_7,contweek_8, contweek_9, contweek_10, contweek_11, contweek_12, contweek_13, contweek_14,contweek_15,contweek_16, contweek_17,
                                         contweek_18, contweek_20, contweek_21, contweek_22, contweek_23, contweek_24, contweek_25,contweek_26, contweek_27, contweek_28, contweek_29, contweek_30, contweek_31, contweek_32, contweek_33, contweek_34, contweek_35,
                                         con2carmode_2, con2carmode_3, con2carmode_4, con2carmode_5, con2carmode_6, con2carmode_7, con2carmode_8, con2carmode_9, con2carmode_10, con2carmode_11, con2carmode_12, con2carmode_13, con2carmode_15, con2carmode_16, con2carmode_17,
                                         con2carmode_18, con2carmode_19, con2carmode_20, con2carmode_21, con2carmode_22, con2carmode_23, con2carmode_24, con2carmode_25, con2carmode_26, con2carmode_27, con2carmode_28, con2carmode_29, con2carmode_30, con2carmode_31, 
                                         con2carmode_32, con2carmode_33, con2carmode_34, con2carmode_35, con2carmode_36, con2carmode_37, con2carmode_38, con2carmode_39, con2carmode_40, con2carmode_41, con2carmode_42, con2carmode_43, con2carmode_44, con2carmode_45, 
                                         con2carmode_46, con2carmode_47, con2carmode_48, con2carmode_49, con2carmode_51, con2carmode_52, con2carmode_53, con2carmode_54, con2carmode_55, con2carmode_56, con2carmode_57, con2carmode_58, con2carmode_59, con2carmode_60, con2carmode_61,
                                         con2year_1951, con2year_1952, con2year_1953, con2year_1954, con2year_1955, con2year_1956, con2year_1957, con2year_1958, con2year_1959, con2year_1960, con2year_1961, con2year_1962, con2year_1963, con2year_1964, con2year_1965, con2year_1966, 
                                         con2year_1967, con2year_1968, con2year_1969, con2year_1970, con2year_1971, con2year_1972, con2year_1973, con2year_1974, con2year_1975, con2year_1976, con2year_1977, con2year_1978, con2year_1979, con2year_1980, con2year_1981, con2year_1982, 
                                         con2year_1983, con2year_1984, con2year_1985, con2year_1986, con2year_1987, con2year_1988, con2year_1989, con2year_1990, con2year_1991, con2year_1992, con2year_1993, con2year_1994, con2year_1995, con2year_1996, con2year_1997, con2year_1998, 
                                         con2year_1999, con2year_2000, con2year_2001, con2year_2002, con2year_2003, con2year_2004, con2year_2005, con2year_2006, con2year_2007, con2n_1, con2n_2, con2n_3, con2n_4, con2n_5, con2n_6, con2n_7, con2n_8, con2n_9, con2n_10, con2n_11, con2n_12, 
                                         con2n_13, con2n_14, con2n_15, con2n_16, con2n_17, con2n_18, con2n_19, con2n_20, con2n_21, con2n_22, con2week_2, con2week_3, con2week_4, con2week_5, con2week_6, con2week_7, con2week_8, con2week_9, con2week_10, con2week_11, con2week_12, con2week_13, 
                                         con2week_14, con2week_15, con2week_16, con2week_17, con2week_18, con2week_20, con2week_21, con2week_22, con2week_23, con2week_24, con2week_25, con2week_26, con2week_27, con2week_28, con2week_29, con2week_30, con2week_31, con2week_32, con2week_33, 
                                         con2week_34, con2week_35))



covariates = data.frame(poly(as.matrix(covariates),degree=2,raw=T))

fit_poly <- lm(data.frame(data_ebay$logbid1,covariates))
summary(fit_poly)

mse2 = mean((predict(fit_poly,covariates[test,]) - data_ebay$logbid1[test])^2)
mse2

# Predication
# data_ebay$poly_model_pred = predict(fit_poly ,data_ebay)

library(Matrix)
library(glmnet)


cv_loop = function(data, a){
  
  folds = 10
  foldindtest <- seq(1,nrow(traindata),length.out=folds+1)
  foldind <- floor(seq(1,nrow(traindata),length.out=folds+1))
  lambdas <- seq(0,1,by = .001)
  mse_totals <- rep(0, times = length(lambdas))
  
  
  for (i in 1:folds){
    
    start <- foldind[i]
    end <- foldind[i+1]
    foldtest <- start:end
    
    # divide up into our (K-1/K)*N to train, and N/K to test
    f_traindata <- traindata[-foldtest,]
    f_testdata <- traindata[foldtest,]
    
    #' remember that GLMNet has issues with data tables.
    f_train_x <- data.matrix(f_traindata[,2:619])
    f_train_y <- data.matrix(f_traindata[,"logbid1"])
    
    f_test_x <- data.matrix(f_testdata[,2:619])
    f_test_y <- data.matrix(f_testdata[,"logbid1"])
    
    # regress with ridge/lasso
    fit = glmnet(f_train_x,f_train_y,alpha=a,lambda=lambdas)
    
    # track the MSE differences in each lambda
    for (j in 1:length(lambdas)){
      mse_totals[j] = mse_totals[j] + sum((predict.glmnet(fit,f_test_x,s=lambdas[j]) - f_test_y)^2)
    }
    
    
  }
  # choose the lambda that returns minimum MSE
  return(lambdas[which.min(mse_totals)])
}

set.seed(0)
smp_size = floor(0.98*nrow(data_ebay))
train_ind <- sample(seq_len(nrow(data_ebay)), size = smp_size)

traindata = data_ebay[train_ind, ]
testdata = data_ebay[-train_ind, ]

# Ridge 
# return our result for the test data
ridge_min <- cv_loop(traindata, a = 0)
final_ridge <- glmnet(data.matrix(testdata[,2:619]),testdata$logbid1,alpha=0,lambda=ridge_min) 
final_ridge$beta

#ridge_min
mse_ridge = mean((predict(final_ridge,data.matrix(testdata[,2:619]),s=ridge_min) -testdata$logbid1)^2)
mse_ridge

# Predication
# data_ebay$ridge_model_pred = predict(final_ridge,data.matrix(testdata[,2:300]),s=ridge_min)


# Lasso
lasso_min <- cv_loop(traindata, a = 1)
final_lasso <- glmnet(data.matrix(testdata[,2:619]),testdata$logbid1,alpha=1,lambda=lasso_min) 
final_lasso$beta


#final_lasso$beta
#lasso_min
mse_lasso = mean((predict(final_lasso,data.matrix(testdata[,2:619]),s=lasso_min) - testdata$logbid1)^2)
mse_lasso

results <- data.frame(Model = c('Linear', 'Poly', 'Ridge', 'Lasso'), MSE = c(mse1, mse2, mse_ridge, mse_lasso))
results

# Crack - mse - 0.95
# dent - mse - 0.86
# broken - mse- 0.85
# rust - mse- 0.71
# contcarmodel - mse- 0.49

# result from logbid1
# Model       MSE
# 1 Linear 0.4374486
# 2   Poly 0.4753682
# 3  Ridge 0.1930542
# 4  Lasso 0.1954250

# result from biddy1

# Model      MSE
# 1 Linear 32419045
# 2   Poly 26767383
# 3  Ridge 10490961
# 4  Lasso 10525398

# _________________________________________________________________________

#1
orig_datah <- orig_data[(orig_data$biddy1<100000),]
min(orig_datah$biddy1)
max(orig_datah$biddy1)
hist(orig_datah$biddy1)

#2
hist(orig_datah$logbid1)



# _________________________________________________________________________

pl <- predict(final_lasso,data.matrix(testdata[,2:619]),s=lasso_min)
pr <- predict(final_ridge,data.matrix(testdata[,2:619]),s=ridge_min)
ob <- testdata$logbid1
mo <- testdata$maker

pred_data <- data.frame(lasso_pred =c(pl), ridge_pred =c(pr), original_bid =c(ob), car_maker=c(mo))
#pred_data

ggp <- ggplot(pred_data, aes(x=car_maker)) + 
  geom_point(aes(y = lasso_pred), color = "darkred", size = 2) +
#  geom_point(aes(y = ridge_pred), color="blue", size = 3) +
  geom_point(aes(y = original_bid), color="yellow", size = 0.5) +
  ggtitle("eBay Auction") +
  xlab("Cars Maker") + 
  ylab("Predicted Value")  + 
  theme(plot.title = element_text(hjust = 0.5))

ggp



