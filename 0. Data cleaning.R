# This is the script that we will use for data cleaning. 
# ----------------
# Instructions
# ----------------
# 1. Please navigate to the 'datacleaning' branch when making changes to this file. 
# 2. Please commit every small change and give a clear description of the change in the commit message
# 3. Please comment on the code as clearly as possible
# --------------------------

library("haven")

# Importing the dataset
# df1 <- read_stata("./112439-V1/temp.dta")
data <- data.frame(df1)
data[1:20,c("ding_good", "ding_bad")]
str(data)
dim(data)
# We have 146734 rows and 548 variables

View(data)
str(data)

# Creating a data set with only raw variables
drops1 <- c("ding_good", "ding_bad", "ding_knowledge", "ding_negation", "ding_low", "ding_high", "ding_pics", 
           "dent_good", "dent_bad", "dent_knowledge", "dent_negation", "dent_low", "dent_high",  "rust_high",
           "dent_pics", "crack_good", "crack_bad", "crack_knowledge", "crack_negation", "crack_low", "crack_high",
           "crack_pics", "problem_good", "problem_bad", "problem_knowledge", "problem_negation", "problem_low",
           "problem_high","problem_pics", "rust_good", "rust_bad", "rust_knowledge", "rust_negation", "rust_low",
           "rust_pics", "scratch_good", "scratch_bad", "scratch_knowledge", "scratch_negation", "scratch_low",
           "scratch_high", "scratch_pics", "broken_good", "broken_bad", "broken_knowledge", "broken_negation",
           "broken_low", "broken_high", "broken_pics", "ding_group", "scratch_group", "crack_group", "broken_group",
           "dent_group", "problem_group", "rust_group")

df2 <- data[ , !(names(data) %in% drops)]

dim(df2)

# Things to do
?read_dta


# -------
# 1) Start working with the author's temp file (after a bit of modification to the code).
#     this solves the problem of creating dummy variables -- AD
# 2) Remove obviously unnecessary columns such as bidders name -- PW
# 3) change the time and date variables to make them suitable for a regression. -- JL
# 4) Remove completely empty columns -- SB
# 5) Ensure variable types is correct. -- YW
# 5) 




?read.dta
