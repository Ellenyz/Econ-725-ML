library(tidyverse) #includes the dplyr and tidyr packages
library(dplyr)
library(tidyr)

#save(orig_data,file='../files for project/orig_data.RData')
load(file='../files for project/orig_data.RData')

#-----------------------
biddy_list <- sprintf("biddy%d",1:22)  ## vector with formatted combination of text and variable values
biddy_str <- paste0(biddy_list,collapse = ',')

desc_data <- orig_data %>% select(biddy_list) 
my.max <- function(x){
  ifelse( !all(is.na(x)), max(x, na.rm=T), NA) 
  }  ## set my own max function to avoid '-INF'

desc_data$max=apply(desc_data,1,FUN=my.max)
desc_data$max  

