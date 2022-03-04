## STA 207
## COVID Project
## Format Oxford data

# I. Setup ----------------------------------------------------------------
#  A. Set working directory
setwd("~/Documents/UC Davis/Winter 2022/STA 207/Final Project/progs/")

#  B. Import packages
require(tidyverse)
require(lubridate)

# II. Data Importing ------------------------------------------------------
#  A. Read in the data
#   1. Oxford
oxford <- read_csv("../raw/OxCGRT_latest.csv")

#   2. WHO dat
who <- read_rds("../data/001_covid.rds")

# III. Data Processing ----------------------------------------------------
#  A. Fix column names
names(oxford) <- str_remove_all(str_replace_all(tolower(names(oxford)), "/ +| +|/", "_"), "\\(|\\)")

#  B. Format date variable 
oxford <- oxford %>% 
	mutate(date = ymd(date))

# IV. Data Analysis -------------------------------------------------------
#  A. Explore school closures
#   1. Summary
closure_smry <- oxford %>% 
	filter(jurisdiction == "NAT_TOTAL") %>% 
	group_by(countryname) %>% 
	summarize(closure = max(ifelse(is.na(c1_school_closing), 0, c1_school_closing), na.rm = T))

#   2. Count types
closure_smry %>% 
	group_by(closure) %>% 
	summarize(cnt = n())
#   closure   cnt
#	1       0     1
# 2       1     3
# 3       2     3
# 4       3   179

#   3. Check non-closure countries
closure_smry %>% 
	filter(closure < 1)

#   countryname  closure
# 1 Comoros            0


#  B. Add features
#   1. First schools closing 
#    i. Look up 
fc_tlkp <- oxford %>% 
	filter(c1_school_closing > 0) %>% 
	group_by(countryname) %>% 
	filter(date==min(date)) %>% 
	select(countryname, first_c1_school_closing = date) %>% 
	unique()

#    ii. Merge on 
oxford <- left_join(oxford, fc_tlkp, by="countryname")

#   2. WHO region
#    i. Look up 
reg_tlkp <- covid %>% 
	select(iso3, who_region) %>% 
	unique()

#    ii. Merge
oxford <- left_join(oxford, reg_tlkp, by = c("countrycode" = "iso3"))

#    iii. Compare
oxford %>% 
	group_by(who_region) %>% 
	summarize(cnt = n())
#   who_region                     cnt
# 1 African Region               33540
# 2 Region of the Americas       97500
# 3 Eastern Mediterranean Region 17160
# 4 European Region              42900
# 5 South-East Asian Region       7020
# 6 Western Pacific Region       39780
# 7 NA                            5460

oxford %>% filter(is.na(who_region)) %>% pull(countryname) %>% unique
# [1] "Hong Kong" "Macao"     "Kosovo"    "Taiwan"

# V. Data Output ----------------------------------------------------------
#  A. Save data
write_rds(oxford %>% select(c(1:10, 50:62)), "../data/002_oxford.rds")

