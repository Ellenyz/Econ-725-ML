#install.packages("stargazer")
library(tidyverse) #includes the dplyr and tidyr packages
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(stargazer)

#save(orig_data,file='../files for project/orig_data.RData')
load(file='../files for project/orig_data.RData')

# Set labels  
orig_data$dealer <- factor(orig_data$dealer, level=c(0,1,NA), labels = c('Seller','Dealer'))
orig_data$inspection <- factor(orig_data$inspection, level=c(0,1,NA), labels = c('Without Inspection','With Inspection'))
orig_data$reserve <- factor(orig_data$reserve, level=c(0,1,NA), labels = c('No Reserve Prive','With Secret Reserve'))
orig_data$buyitnow <- factor(orig_data$buyitnow, level=c(0,1,NA), labels = c('No BUYITNOW Option','With BUYITNOW Option'))

#-----------------------
# Part 1 Summary Statistics (Proportion of services provided, grouping by seller/dealer)
desc_data <- orig_data %>% select(dealer,inspection, reserve, buyitnow) %>%
  na.omit() %>% 
  group_by(dealer) ##group by dealer and private seller 

type_seller <- desc_data %>% summarise(Number_of_sellerdealer=n())

# Get Descriptive Tables
buyoptions_vec <- c('inspection', 'reserve', 'buyitnow')
table <- NULL
for (i in 1:3){
  table_i <- table(desc_data[c(buyoptions_vec[i],'dealer')])
  table_i <- round(prop.table(table_i),4)
  table <- rbind(table,table_i)
}
table <- t(table)
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
## I found a great amount of obs have negative margin <- (winningbid - bookvalue)

sum <- biddy %>% group_by(dealer) %>% 
  dplyr::summarize(mean_numbids = mean(numbids, na.rm = TRUE),
                    mean_totalsold = mean(totalsold, na.rm = TRUE),
                    mean_totallisted = mean(totallisted, na.rm = TRUE),
                    mean_bookvalue = mean(bookvalue, na.rm = TRUE),
                    mean_winningbid = mean(winningbid, na.rm = TRUE))
sum <- data.frame(sum)
print('Sellers have great lower number of bids, sales, total listed and bookvalue on average comparing with dealers.')


#-----------------------------------------
# Plotting
##1st plt
plt1 <- ggplot(desc_data, aes(dealer)) + ggtitle("Distribution of Seller/Dealer") +  geom_bar() +
  xlab("Type of Sellers") + ylab("Number of Obs")

##2nd plt
plt2 <- barplot(table[,c(2,4,6)], main="Distribution of Options",
              ylab="Proportion", col=c("darkblue","red"), ylim=range(0,0.4),
              legend = rownames(table), beside=TRUE, cex.axis = 0.8, cex.lab=0.8, cex.names = 0.8)
text(plt2,table[,c(2,4,6)],table[,c(2,4,6)],cex=1,pos=3)

##3rd plt
sum <- reshape2::melt(sum,id.vars='dealer')
sum$variable <- factor(sum$variable,labels=c('Mean Number of Bids','Mean Total Sold',
                                             'Mean Total Listed','Mean Bookvalue','Mean Winning Bid'))

plt3 <- ggplot(sum[1:6,],aes(variable,value,fill=dealer)) +
  geom_bar(stat="identity",position="dodge") + ggtitle("Number of Sales and Lists by Seller/Dealer") +
  geom_text(aes(label = round(sum[1:6,]$value,2), y = sum[1:6,]$value + 1), position = position_dodge(width = 1)) + 
  xlab('Variables') + ylab('Average Numbers')
 

##4th plt
plt4 <- ggplot(sum[7:10,],aes(variable,value,fill=dealer)) +
  geom_bar(stat="identity",position="dodge") + ggtitle("Bookvalue and Winning Bid by Seller/Dealer") +
  geom_text(aes(label = round(sum[7:10,]$value,2), y = sum[7:10,]$value + 300), position = position_dodge(width = 1)) + 
  xlab('Variables') + ylab('US Dollar')



#Out for LaTex:
#stargazer(table)






