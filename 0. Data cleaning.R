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


