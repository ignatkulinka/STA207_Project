## STA 207
## COVID Project
## Create data models

# I. Setup ----------------------------------------------------------------
#  A. Set working directory
setwd("~/Documents/UC Davis/Winter 2022/STA 207/Project/progs")

#  B. Import packages
require(tidyverse)
require(data.table)
require(plm)
require(lmtest)
require(sandwich)

#  C. Options 
options(dplyr.summarise.inform=F) 

# II. Data Importing ------------------------------------------------------
#  A. Read in the combined data
full_dt <- readRDS("../data/100_cmb.rds")


# III. Data Processing ----------------------------------------------------
#  A. Subset data 
#   1. Subset to rows with non missing values and columns of interest 
cmb <- full_dt %>% 
	select(date_reported, country, new_cases, c1_school_closing, c2_workplace_closing, c3_cancel_public_events, c4_restrictions_on_gatherings, c5_close_public_transport, c6_stay_at_home_requirements, who_region) %>% 
	filter(!is.na(c1_school_closing), !is.na(c2_workplace_closing), !is.na(new_cases))

#   2. Subset to top 10 countries by population
#    i. Maka look up table
top10_tlkp <- full_dt %>% 
	filter(!is.na(country)) %>% 
	group_by(country, pop) %>% 
	summarize(cnt = n()) %>% 
	arrange(-pop) %>% 
	head() %>% 
	select(country, pop)

#    ii. Subset
cmb <- cmb %>% 
	inner_join(., top10_tlkp, by = "country")

#    iii. Look for any missing data spots
cmb %>% 
	group_by(country) %>% 
	mutate(diff = as.numeric(dplyr::lead(date_reported) - date_reported)) %>% 
	group_by(country, diff) %>% 
	summarize(cnt = n()) %>% 
	filter(diff>1)
# Note: appears to be no gaps 

#    iv. Check for min/max
cmb %>% 
	group_by(country) %>% 
	summarize(min_dt = min(date_reported),
											max_dt = max(date_reported))
#       country                  min_dt     max_dt    
#	1 Bangladesh               2020-01-03 2022-02-12
# 2 Brazil                   2020-01-03 2022-02-12
# 3 China                    2020-01-03 2022-02-16
# 4 India                    2020-01-03 2022-02-07
# 5 Indonesia                2020-01-03 2022-01-31
# 6 Mexico                   2020-01-03 2022-02-14
# 7 Nigeria                  2020-01-03 2022-01-31
# 8 Pakistan                 2020-01-03 2022-01-30
# 9 Russian Federation       2020-01-03 2022-02-06
# 10 United States of America 2020-01-03 2022-02-15
# Note: the various end dates! 

#  B. Define cases per 100,000
#   1. Make new field
cmb <- cmb %>% 
	mutate(norm_new_cases = new_cases/pop * 100000)

#   1. Any measure - if flg > 0 then 1 else 0
cmb <- cmb %>% 
	mutate(asc_c1 = ifelse(c1_school_closing > 0, 1, 0),
								asc_c2 = ifelse(c2_workplace_closing > 0, 1, 0),
								asc_c3 = ifelse(c3_cancel_public_events > 0, 1, 0),
								asc_c4 = ifelse(c4_restrictions_on_gatherings > 0, 1, 0),
								asc_c5 = ifelse(c5_close_public_transport > 0, 1, 0),
								asc_c6 = ifelse(c6_stay_at_home_requirements > 0, 1, 0))

#   2. Max measure - if flg == 3 then 1 else 0
# three2 <- three2 %>% 
# 	mutate(msc_c1 = ifelse(c1_school_closing == 3, 1, 0),
# 								msc_c2 = ifelse(c2_workplace_closing == 3, 1, 0))

#  D. Any measure: Add 7 day lag
#   1. Make a look up of all lockdown start/end dates
#    i. School closing
adj_c1_tlkp <- cmb %>%
	group_by(country, ord = data.table::rleid(asc_c1 == 1)) %>%
	filter(asc_c1 == 1) %>% 
	group_by(country, ord) %>% 
	summarize(min_dt = min(date_reported), max_dt = max(date_reported)) %>% 
	select(ord, country, min_dt, max_dt) %>% 
	mutate(min_dt_adj = min_dt + 7,
								max_dt_adj = max_dt + 7) %>% 
	as.data.table()

adj_c1_tlkp2 <- cmb_dt %>%
	group_by(country) %>%
	filter(asc_c1 == 1) %>% 
	group_by(country, ord) %>% 
	summarize(min_dt = min(date_reported), max_dt = max(date_reported)) %>% 
	select(ord, country, min_dt, max_dt) %>% 
	mutate(min_dt_adj = min_dt + 7,
								max_dt_adj = max_dt + 7) %>% 
	as.data.table()

# checks 
adj_c1_tlkp %>% 
	filter(min_dt_adj > max_dt_adj)

#    ii. Workplace
adj_c2_tlkp <- cmb %>%
group_by(country, ord = data.table::rleid(asc_c2 == 1)) %>%
	filter(asc_c2 == 1) %>% 
	group_by(country, ord) %>% 
	summarize(min_dt = min(date_reported), max_dt = max(date_reported)) %>% 
	select(country, min_dt, max_dt) %>% 
	mutate(min_dt_adj = min_dt + 7,
								max_dt_adj = max_dt + 7) %>% 
	as.data.table()

# checks 
adj_c2_tlkp %>% 
	filter(min_dt_adj > max_dt_adj)

#   2. Merge back on 
cmb <- cmb %>% as.data.table()

#    i. School closing
cmb[adj_c1_tlkp, on=c("country"="country", "date_reported>=min_dt_adj", "date_reported<=max_dt_adj"), asc_c1_7:=1]
cmb[is.na(asc_c1_7), asc_c1_7 := 0]

#    ii. Workplace closing
cmb[adj_c2_tlkp, on=c("country"="country", "date_reported>=min_dt_adj", "date_reported<=max_dt_adj"), asc_c2_7:=1]
cmb[is.na(asc_c2_7), asc_c2_7 := 0]

#  D. Add year 
cmb <- cmb %>% 
	mutate(yr = year(date_reported))

#  E. Create panel data frame
tmp <- pdata.frame(cmb, index=c("country", "date_reported"), drop.index=TRUE, row.names=TRUE)

# Negative values of new cases
# cmb <- cmb %>% 
# 	filter(norm_new_cases < 0) %>% 
# 	mutate(norm_new_cases = NA)


# IV. Data Analysis -------------------------------------------------------
#  0. Process to inspect lag
#   i. Create

#  A. Fit model: Any measure: Add 7 day lag
#   1. OLS
ols <-lm(new_cases ~ factor(country) + factor(asc_c1_7) + factor(asc_c2_7), data=cmb)
summary(ols)

#   2. Panel - fixed effects
m_fixed <- plm(norm_new_cases ~ factor(asc_c1) + factor(asc_c2),
															data=tmp, model="within")
summary(m_fixed)

#   3. Compare OLS and Panel-fixed
pFtest(m_fixed, ols)
# significant 

#   4. Panel - random effects
random <- plm(norm_new_cases ~ factor(asc_c1) + factor(asc_c1_7) + factor(asc_c2) + factor(asc_c2_7), data=tmp, model="random")
summary(random)

#   5. Fixed or Random: Hausman test
phtest(random, m_fixed)
# random

#   6. 
# pool <- plm(new_cases ~ factor(asc_c1_7)  + factor(asc_c2_7), data=cmb, index=c("country", "date_reported"), model="pooling")
# plmtest(pool, type=c("bp"))

#   7. Test for serial correlation
pbgtest(random)
# serial correlation
bgtest(random)

#   8. test for constant variance
bptest(new_cases ~ factor(asc_c1_7)  + factor(asc_c2_7) + factor(country), data = cmb, studentize=F)
# Presence of heteroskedasticity 

cross sectional dependence
pcdtest()


#   9. Robust covariance matrix estimation (Sandwich estimator)
# coeftest(random, sandwich(vcovHC(random, type = "HC5"))

# V. Data Output ----------------------------------------------------------



