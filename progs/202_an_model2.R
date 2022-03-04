## STA 207
## COVID Project
## Create data models

# I. Setup ----------------------------------------------------------------
#  A. Set working directory
setwd("~/Documents/UC Davis/Winter 2022/STA 207/Project/progs")

#  B. Import packages
require(tidyverse)
require(data.table)
require(zoo)
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
	select(date_reported, country, new_cases, c1_school_closing, c2_workplace_closing, who_region, first_case) %>% 
	filter(!is.na(c1_school_closing), !is.na(c2_workplace_closing), !is.na(new_cases)) %>% 
	mutate(country = as.factor(country))

#   2. Subset to top 10 countries by population
#    i. Make look up table
top_tlkp <- full_dt %>% 
	filter(!is.na(country)) %>% 
	group_by(country, pop) %>% 
	summarize(cnt = n()) %>%
	filter(pop > 100000) %>% 
	select(country, pop)

#    ii. Subset
cmb <- cmb %>% 
	inner_join(., top_tlkp, by = "country")

#    iii. Look for any missing data spots
cmb %>% 
	group_by(country) %>% 
	mutate(diff = as.numeric(dplyr::lead(date_reported) - date_reported)) %>% 
	group_by(country, diff) %>% 
	summarize(cnt = n()) %>% 
	filter(diff>1)
# Note: appears to be gaps 

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

#  D. Keep in the predictors of choice (any effect scenario)
cmb <- cmb %>% 
	mutate(asc_c1 = as.factor(ifelse(c1_school_closing > 0, 1, 0)),
								asc_c2 = as.factor(ifelse(c2_workplace_closing > 0, 1, 0)))

#  E. Add fields
#   1. Year
cmb <- cmb %>% 
	mutate(yr = as.factor(year(date_reported)))

#   2. Define cases per 100,000
#    i. Make new field
cmb <- cmb %>% 
	mutate(norm_new_cases = new_cases/pop * 100000)

#    ii. Define a rolling average of cases per 100,000
# mk_roll <- function(x, k) mutate(x, "norm_new_cases_{{k}}" := rollmean(norm_new_cases, k, fill = NA)) 
# cmb <- cmb %>% 
# 	group_by(country) %>% 
# 	mutate(roll_new_cases_3 = rollmean(norm_new_cases, 3, fill = NA))

#   3. Days since first infection 
cmb <- cmb %>% 
	group_by(country) %>% 
	mutate(days_since_fc = (ifelse(as.numeric(date_reported - first_case) > 0, as.numeric(date_reported - first_case), 0)))

#   4. Day of the week dummy
cmb <- cmb %>% 
	mutate(day_of_week = as.factor(weekdays(date_reported)))

#   5. Lag policies (3)
cmb <- lag_fitter(cmb, 7)

#   6. Factor country
cmb <- cmb %>% 
	mutate(country = as.factor(country))

#  F. Investigate new cases
cmb <- cmb %>% mutate(ln_new_cases = log(new_cases - min(new_cases)),
																						ln_norm_new_cases = log(norm_new_cases- min(new_cases)))


ggplot(cmb, aes(x = date_reported, y = norm_new_cases, color = country)) +
	geom_line() 
	
ggplot(cmb, aes(x = date_reported, y = ln_new_cases, color = country)) +
	geom_line()

ggplot(cmb, aes(x = date_reported, y = ln_norm_new_cases, color = country)) +
	geom_line()

#  E. Create panel data frame
# tmp <- pdata.frame(cmb, index=c("country", "date_reported"), drop.index=TRUE, row.names=TRUE)

# Negative values of new cases
# cmb <- cmb %>% 
# 	filter(norm_new_cases < 0) %>% 
# 	mutate(norm_new_cases = NA)


# IV. Data Analysis -------------------------------------------------------
#  A. Fit fixed effects model
m_fixed <- plm(new_cases ~ asc_c1 + asc_c1_1 + asc_c1_2 + asc_c1_3 + asc_c1_4 + asc_c1_5 + asc_c1_6 + asc_c1_7 +
																asc_c2 + asc_c2_1 + asc_c2_2 + asc_c2_3 + asc_c2_4 + asc_c2_5 + asc_c2_6 + asc_c2_7 +
																country, data=cmb, index=c("country", "date_reported"), model="within")
summary(m_fixed)

m_fixed2 <- plm(norm_new_cases ~ asc_c1 + asc_c1_1 +
																asc_c2 + asc_c2_1 +
																country, data=cmb, index=c("country", "date_reported"), model="within")
summary(m_fixed2)

m_fixed2 <- plm(ln_norm_new_cases ~ asc_c1 + asc_c1_2 + asc_c2 + asc_c2_1 + 
																	country, data=cmb, index=c("country", "date_reported"), model="within")
summary(m_fixed2)

m_fixed2 <- plm((ln_new_cases) ~ asc_c1 + asc_c1_7 + 
																	asc_c2 + asc_c2_7+
																	country, data=cmb, index=c("country", "date_reported"), model="within")
summary(m_fixed2)


# V. Data Output ----------------------------------------------------------



