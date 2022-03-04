## STA 207
## COVID Project
## Format WHO Data

# I. Setup ----------------------------------------------------------------
#  A. Set working directory
setwd("~/Documents/UC Davis/Winter 2022/STA 207/Final Project/progs/")

#  B. Import packages
require(tidyverse)
require(openxlsx)

# II. Data Importing ------------------------------------------------------
#  A. Read in the data
#   1. COVID
covid <- read_csv("../raw/WHO-COVID-19-global-data.csv")

#   2. World Bank population
pop <- read_csv("../raw/API_SP.POP.TOTL_DS2_en_csv_v2_3628828.csv", skip = 3)

#   3. Country codes
cds_tlkp <- read.xlsx("../raw/country_codes.xlsx", na.strings = NULL)
names(cds_tlkp) <- c("country_name", "iso2", "iso3", "numeric")
# https://www.iban.com/country-codes

# III. Data Processing ----------------------------------------------------
#  A. Data formating
#   1. Column names
names(covid) <- tolower(names(covid))

#   2. Update who regions
who_reg <- c("AFRO", "AMRO", "EMRO", "EURO", "Other", "SEARO", "WPRO")
who_reg2 <- c("African Region", "Region of the Americas", "Eastern Mediterranean Region",
														"European Region", "Other", "South-East Asian Region", "Western Pacific Region")
covid <- covid %>% 
	mutate(who_region = factor(who_region, levels = who_reg, labels = who_reg2))

#   3. Add iso3 code
#    i. Merge on the codes
covid <- covid %>% 
	left_join(., cds_tlkp[, c("iso2", "iso3")], by=c("country_code"="iso2")) 

#    ii. Fix Namibia
covid <- covid %>% 
	mutate(iso3 = ifelse(country == "Namibia", "NAM", iso3))

#    iii. Check missing
covid %>% filter(is.na(iso3)) %>% group_by(country, country_code, iso3) %>% summarize(cnt = n())
#   country        country_code   cnt
# 1 Bonaire        XA           NA      777
# 2 Kosovo[1]      XK           NA      777
# 3 Other          NA           NA      777
# 4 Saba           XC           NA      777
# 5 Sint Eustatius XB           NA      777
# NOTE: Bonair, Saba, Sint Eustatius share a code BES: not included (outside of scope)

#    iv. Check that countries - iso3 is one to one
covid %>%  
	group_by(country) %>% 
	summarize(cnt_iso3 = n_distinct(iso3)) %>% 
	filter(cnt_iso3 > 1)
# 0 

#   4. Add population
#    i. Merge on data
covid <- covid %>% 
	left_join(., pop %>% select(iso3 = `Country Code`, pop = `2020`), by=c("iso3"))

#    ii. Check which countries do not have population
covid %>% 
	filter(is.na(pop)) %>% 
	group_by(country) %>% 
	summarize(cnt = n())
# 25 minor countries most likely out of scope

#  B. Summary numbers 
#   1. Total countries
covid %>% 
	summarize(n_distinct(country))
# 237 countries

#   2. Countries without cases/deaths recorded
covid %>% 
	group_by(country) %>% 
	summarize(tot_new_cases = sum(new_cases),
											tot_cum_cases = sum(cumulative_cases), 
											tot_new_deaths = sum(new_deaths), 
											tot_cum_deaths = sum(cumulative_deaths)) %>% 
	filter((tot_new_cases) == 0) 

#  C. Add features
#   1. First case 
#    i. Look up 
fc_tlkp <- covid %>% 
	filter(new_cases > 0) %>% 
	group_by(country) %>% 
	filter(date_reported==min(date_reported)) %>% 
	select(country, first_case = date_reported) %>% 
	unique()

#    ii. Merge on 
covid <- left_join(covid, fc_tlkp, by="country")

#   2. First death
#    i. Look up  
fd_tlkp <- covid %>% 
	filter(new_deaths > 0) %>% 
	group_by(country) %>% 
	filter(date_reported==min(date_reported)) %>% 
	select(country, first_death = date_reported) %>% 
	unique()

#    ii. Merge on 
covid <- left_join(covid, fd_tlkp, by="country")

#   3. Case and death rate
covid <- covid %>% 
	mutate(case_rate = cumulative_cases / pop * 100000,
								death_rate = cumulative_deaths / pop * 100000)

# IV. Data Analysis -------------------------------------------------------
#  A. Check data consistency 
covid %>% 
	arrange(country, date_reported) %>% 
	group_by(country) %>% 
 mutate(dt_diff = as.numeric(date_reported - lag(date_reported))) %>% 
	group_by(country, dt_diff) %>% 
	summarize(cnt = n()) %>% 
	filter(cnt > 1, cnt != 775)
# 776 daily records for each country 

#  B. Time between first case and first death 
#   1. All countries
covid %>% 
	group_by(country) %>% 
	summarize(fc_fd_diff = as.numeric(first_death - first_case)) %>% 
	group_by(country, fc_fd_diff) %>% 
	summarize(n())

#   2. Average 
covid %>% 
	group_by(country, who_region) %>% 
	summarize(fc_fd_diff = as.numeric(first_death - first_case)) %>% 
	filter(!is.na(fc_fd_diff)) %>% 
	group_by(who_region) %>% 
	summarize(mean(fc_fd_diff))




# V. Data Output ----------------------------------------------------------
#  A. Save COVID data
write_rds(covid, "../data/001_covid.rds")












