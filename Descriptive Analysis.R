#install.packages("stargazer")
library(tidyverse) #includes the dplyr and tidyr packages
library(dplyr)
library(tidyr)
library(data.table)
library(stargazer)

#save(orig_data,file='../files for project/orig_data.RData')
load(file='../files for project/orig_data.RData')

#-----------------------
# Part 1 Summary Statistics (Proportion of services provided, grouping by seller/dealer)
buyoptions_vec <- c('inspection','reserve','buyitnow')

desc_data <- orig_data %>% select(dealer,all_of(buyoptions_vec)) %>%
  na.omit() %>%
  group_by(dealer)  ##group by dealer and private seller 

# Set labels  
desc_data$dealer <- factor(desc_data$dealer, labels = c('Seller','Dealer'))
desc_data$inspection <- factor(desc_data$inspection,labels = c('Without Inspection','With Inspection'))
desc_data$reserve <- factor(desc_data$reserve,level=c(0,1), labels = c('No Reserve Prive','With Secret Reserve'))
desc_data$buyitnow <- factor(desc_data$buyitnow,level=c(0,1), labels = c('No BUYITNOW Option','With BUYITNOW Option'))
# Get Descriptive Tables
table <- NULL
for (i in 1:3){
  table_i <- table(desc_data[c(buyoptions_vec[i],'dealer')])
  table_i <- round(prop.table(table_i),4)
  table <- rbind(table,table_i)
}
data.frame(table)

#-------------------------
# Part 2 
biddy_list <- sprintf("biddy%d",1:22)  ## vector with formatted combination of text and variable values

my.max <- function(x){
  ifelse(!all(is.na(x)), max(x, na.rm=T), NA)
}  ## set my own max function to avoid '-INF'

biddy <- orig_data %>% select(biddy_list) 
biddy <- biddy %>% mutate(winningbid=apply(biddy,1,FUN=my.max)) %>% 
  mutate(winningbid=na_if(winningbid,NA)) 
##Testing:
##biddy$winningbid
##i <- sample(1:length(biddy),1)
##stopifnot(biddy$winningbid[i]==my.max(biddy[i,-23]))

biddy <- orig_data %>% select('dealer','reserve','bookvalue','numbids','startbid','totallisted','totalsold') %>% 
  add_column(biddy['winningbid']) %>%
  mutate(margin=winningbid - bookvalue)
biddy
## I found a great amount of obs have negative margin <- (winningbid - bookvalue)







