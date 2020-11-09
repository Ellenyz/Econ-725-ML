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

df1 <- read_dta("temp.dta")
orig_data <- data.frame(df1)


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


