# This is the script that we will use for data cleaning. 
# ----------------
# Instructions
# ----------------
# 1. Please navigate to the 'datacleaning' branch when making changes to this file. 
# 2. Please commit every small change and give a clear description of the change in the commit message
# 3. Please comment on the code as clearly as possible
# --------------------------

library("foreign")

# Importing the dataset
df1 <- read.dta("./112439-V1/ebaydatafinal.dta", convert.dates = T)

dim(df1)
# We have 146734 rows and 548 variables

View(df1)

str(df1)
# Things to do 
# -------
# 1) Start working with the author's temp file (after a bit of modification to the code).
#     this solves the problem of creating dummy variables -- AD
# 2) Remove obviously unnecessary columns such as bidders name -- PW
# 3) change the time and date variables to make them suitable for a regression. -- JL
# 4) Remove completely empty columns -- SB
# 5) Ensure variable types is correct. -- YW
# 5) 




?read.dta
