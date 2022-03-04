## STA 207
## COVID Project
## Combine oxford and WHO datasets

# I. Setup ----------------------------------------------------------------
#  A. Set working directory
setwd("~/Documents/UC Davis/Winter 2022/STA 207/Final Project/progs/")

#  B. Import packages
require(tidyverse)
require(lubridate)

# II. Data Importing ------------------------------------------------------
#  A. Read in the data
#   1. Oxford
oxford <- read_rds("../data/002_oxford.rds")

#   2. WHO dat
who_dt <- read_rds("../data/001_covid.rds")


# III. Data Processing ----------------------------------------------------
#  A. Subset oxford to matching fields and "c" variables
oxford2 <- oxford %>% 
	filter(jurisdiction == "NAT_TOTAL") %>% 
	select(countryname, countrycode, ox_dt = date, matches("c[0-9]")) %>% 
	mutate(ox_flg = 1)


# IV. Data Analysis -------------------------------------------------------
#  A. Combine WHO and oxford data sets
#   1. Check for overlap between country codes 
#    i. Prep for mrge
who_iso3 <- who_dt %>%
	group_by(iso3) %>% 
	summarize(cnt = n())
# 233 countries

oxford_iso3 <- oxford %>% 
	group_by(countrycode) %>% 
	summarize(cnt = n())
# 186 countries

#    ii. Merge
cmb <- merge(who_dt, oxford2, by.x = c("iso3", "date_reported"), by.y = c("countrycode", "ox_dt"), all.x = T)

#    iii. Compare
cmb %>% 
	filter(is.na(ox_flg)) %>% 
	group_by(country) %>% 
	summarize(dt_min=min(date_reported),
											dt_max=max(date_reported)) 
# Note: 55 countries missing oxford data

cmb %>% 
	filter(is.na(ox_flg) & who_region == "European Region") %>% 
	group_by(country) %>% 
	summarize(dt_min=min(date_reported),
											dt_max=max(date_reported)) 
#       country         dt_min     dt_max    
# 1 Armenia         2020-01-03 2022-02-17
# 2 Gibraltar       2020-01-03 2022-02-17
# 3 Guernsey        2020-01-03 2022-02-17
# 4 Holy See        2020-01-03 2022-02-17
# 5 Isle of Man     2020-01-03 2022-02-17
# 6 Jersey          2020-01-03 2022-02-17
# 7 Kosovo[1]       2020-01-03 2022-02-17
# 8 Montenegro      2020-01-03 2022-02-17
# 9 North Macedonia 2020-01-03 2022-02-17
# Note: 9 countries in the european region


# V. Data Output ----------------------------------------------------------
#  A. Save data
write_rds(cmb, "../data/100_cmb.rds")

