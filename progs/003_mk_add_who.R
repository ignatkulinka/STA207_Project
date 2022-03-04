## STA 207
## COVID Project
## Review additional who data

# I. Setup ----------------------------------------------------------------
#  A. Set working directory
setwd("~/Documents/UC Davis/Winter 2022/STA 207/Final Project/progs/")

#  B. Import packages
require(tidyverse)
require(lubridate)

# II. Data Importing ------------------------------------------------------
#  A. Read in the data
add <- read_csv("../raw/data_unpd_org_all.csv")

# III. Data Processing ----------------------------------------------------
#  A. Format the data column names
names(add) <- str_remove_all(str_replace_all(tolower(names(add)), "/ +| +|/", "_"), "\\(|\\)|\\-")

#  B. Format year
#   1. Add year
add <- add %>% 
	mutate(yr = mdy(substr(year, 1, 10)))

#   2. Compare
# add %>% 
# 	group_by(year, yr) %>% 
# 	summarize(cnt = n())
# NOTE: unknown is kept as NA

# IV. Data Analysis -------------------------------------------------------
#  A. Review indicators in the data 
add %>% 
	group_by(indicator, indicator_description) %>% 
	summarize(cnt = n())


# V. Data Output ----------------------------------------------------------
#  A. Save data
write_rds(add, "../data/003_add_who.rds")
