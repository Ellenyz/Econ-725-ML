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
orig_data <- data.frame(df1)

dim(orig_data)
# We have 146734 rows and 605 variables. 

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





# -------
# 1) Start working with the author's temp file (after a bit of modification to the code).
#     this solves the problem of creating dummy variables -- AD [done]
# 2) Remove obviously unnecessary columns such as bidders name -- PW
# 3) change the time and date variables to make them suitable for a regression. -- JL
# 4) Remove completely empty columns -- SB
# 5) Ensure variable types is correct. -- YW
# 5) 




?read.dta
