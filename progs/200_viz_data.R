## STA 207
## COVID Project
## Plot WHO data

# I. Setup ----------------------------------------------------------------
#  A. Set working directory
setwd("../ignat/Documents/UC Davis/Winter 2022/STA 207/Project/progs")

#  B. Import packages
require(tidyverse)
require(countrycode)
require(RColorBrewer)

# II. Data Importing ------------------------------------------------------
#  A. Read in the data
#   1. COVID
covid <- read_rds("../data/001_covid.rds")

#   2. Oxford
oxford <- read_rds("../data/002_oxford.rds")


# III. Data Processing ----------------------------------------------------

# IV. Data Analysis -------------------------------------------------------
#  A. Charts for WHO data
#   1. Average weekly cases by WHO region
#    i. Aggregate data
agg1 <- covid %>% 
	filter(who_region != "Other") %>% 
	mutate(weekly = ymd(cut(date_reported, breaks = "1 week"))) %>% 
	select(who_region, weekly, new_cases) %>% 
	group_by(who_region, weekly) %>% 
	summarize(avg_cases = mean(new_cases))

#    ii. Plot data
ggplot(agg1, aes(x=weekly, y=avg_cases, color=who_region)) +
	geom_line(stat="identity", alpha = 0.7) +
	scale_x_date(name = "Date Reported", 
														date_breaks = "3 month",
														date_minor_breaks = "1 month",
														date_labels = "%m-%Y") +
	scale_color_manual(values = brewer.pal(n = 6, name = "Dark2"))+
	theme_classic() +
	scale_y_continuous(name = "Average Number of Cases", labels = scales::comma_format()) +
	labs(title = "Weekly Average New Cases by WHO Region",
						color = "WHO Region") +
	theme(legend.position = "bottom",
							plot.title = element_text(hjust = 0.5, size = 14))

#   B. Average weekly deaths by WHO region
#    i. Aggregate data
agg2 <- covid %>% 
	filter(who_region != "Other") %>% 
	mutate(weekly = ymd(cut(date_reported, breaks = "1 week"))) %>% 
	select(who_region, weekly, new_deaths) %>% 
	group_by(who_region, weekly) %>% 
	summarize(avg_cases = mean(new_deaths))

#    ii. Plot data
ggplot(agg2, aes(x=weekly, y=avg_cases, color=who_region)) +
	geom_line(stat="identity", alpha = 0.7) +
	scale_x_date(name = "Date Reported", 
														date_breaks = "3 month",
														date_minor_breaks = "1 month",
														date_labels = "%m-%Y") +
	scale_color_manual(values = brewer.pal(n = 6, name = "Dark2"))+
	theme_classic() +
	scale_y_continuous(name = "Average Number of Deaths", labels = scales::comma_format()) +
	labs(title = "Weekly Average New Deaths by WHO Region",
						color = "WHO Region") +
	theme(legend.position = "bottom",
							plot.title = element_text(hjust = 0.5, size = 14))

#   C. Average case rate by region
#    i. Aggregate data
agg3 <- covid %>% 
	filter(who_region != "Other") %>% 
	mutate(weekly = ymd(cut(date_reported, breaks = "1 week"))) %>% 
	select(who_region, weekly, new_cases, pop) %>% 
	group_by(who_region, weekly) %>% 
	summarize(avg_cases = sum(new_cases, na.rm = T)/sum(pop, na.rm = T) * 100000)

#    ii. Plot data
ggplot(agg3, aes(x=weekly, y=avg_cases, color=who_region)) +
	geom_line(stat="identity", alpha = 0.7) +
	scale_x_date(name = "Date Reported", 
														date_breaks = "3 month",
														date_minor_breaks = "1 month",
														date_labels = "%m-%Y") +
	scale_color_manual(values = brewer.pal(n = 6, name = "Dark2")) +
	theme_classic() +
	scale_y_continuous(name = "Case Rate per 100,000", labels = scales::comma_format()) +
	labs(title = "Weekly Case Rate per 100,000 by WHO Region",
						color = "WHO Region") +
	theme(legend.position = "bottom",
							plot.title = element_text(hjust = 0.5, size = 14))

#  2. Charts for Oxford Data
#   A. Initial school closing for all 
#    i. Aggregate data
agg_ox <- oxford %>% 
	select(countryname, first_c1_school_closing) %>% 
	group_by(first_c1_school_closing) %>% 
	summarize(cnt = n_distinct(countryname))

#    ii. Plot data
ggplot(agg_ox, aes(x=first_c1_school_closing, y=cnt)) +
	geom_line() +
	# geom_vline(xintercept = ymd("2020-03-16")) +
	scale_x_date(name = "Date of Intervention", 
														date_breaks = "1 week",
														date_labels = "%d-%m-%Y",
														limits = c(ymd("2020/2/01"), ymd("2020/4/01"))) +
	theme_classic() +
	scale_y_continuous(name = "Number of Countries", labels = scales::comma_format()) +
	labs(title = "Date of Initial Schools Intervention") +
	theme(plot.title = element_text(hjust = 0.5, size = 14))

#   B. Initial school closing by who region
#    i. Aggregate data
agg_ox2 <- oxford %>% 
	select(countryname, who_region, first_c1_school_closing) %>% 
	group_by(first_c1_school_closing, who_region) %>% 
	summarize(cnt = n_distinct(countryname)) %>% 
	filter(who_region %in% c("European Region", "African Region"))

#    ii. Plot data
ggplot(agg_ox2, aes(x=first_c1_school_closing, y=cnt, fill=who_region)) +
	geom_bar(stat="identity") +
	# geom_vline(xintercept = ymd("2020-03-16")) +
	scale_x_date(name = "Date of Intervention", 
														date_breaks = "1 week",
														date_labels = "%d-%m-%Y",
														limits = c(ymd("2020/2/20"), ymd("2020/4/01"))) +
	theme_classic() +
	scale_y_continuous(name = "Number of Countries") +
	scale_fill_manual(values = brewer.pal(n = 2, name = "Dark2")) +
	labs(title = "Date of Initial Schools Intervention\nfor African And European WHO Regions",
						fill = "WHO Region") +
	theme(legend.position = "bottom",
							plot.title = element_text(hjust = 0.5, size = 14))

# V. Data Output ----------------------------------------------------------









