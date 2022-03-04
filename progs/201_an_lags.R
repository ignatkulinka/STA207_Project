## STA 207
## COVID Project
## Lag fitting functions

# A. Make a look up in the combined table for specified field
mk_tlkp <- function(x, field, lag_days) {
	x <- as.data.table(x)
	x[, ord := data.table::rleid(get(field) == 1), country]
	tlkp <- x[get(field) == 1, .(min_dt = min(date_reported), max_dt = max(date_reported)), .(country, ord)]
	tlkp[, min_dt_adj := min_dt + lag_days]
	tlkp[, max_dt_adj := max_dt + lag_days]

	return(tlkp)
}

# B. Adds a prespecified lagged variables to the data set
lag_adder <- function(x, lag_days = 14){
	# 1. Make a look up of all lockdown start/end dates
	x <- as.data.table(x)
	tlkp <- list()
	for (i in names(x)[str_detect(names(x), "asc_c") & nchar(names(x)) == 6]) {
		tlkp[[i]] <- mk_tlkp(x, i, lag_days)
		x[, (paste0(i, "_", lag_days)) := 0]
		x[tlkp[[i]], on=c("country"="country", "date_reported>=min_dt_adj", "date_reported<=max_dt_adj"), paste0(i, "_", lag_days):=1]
		
	}
	return(x)
}

# C. Fit all n models and save results
lag_fitter <- function(y, n, response="norm_new_cases") {
	# 1. Add up to n lags and on each iteration fit a model
	for (i in 1:n) {
		y <- lag_adder(y, i)
	}
	
	# 2. Create model equation and panel data
	#  i. Convert to factors 
	y <- y %>%
		mutate(across(starts_with("asc"), as.factor))
	
	# #  ii. Make panel data 
	# tmp <- pdata.frame(y, index=c("country", "date_reported"), drop.index=TRUE, row.names=TRUE)
	# 
	# #  iii. Make equation
	# m_eq <- as.formula(paste0(response, "~ ", paste0(names(y)[str_detect(names(y), "asc_c")], collapse = " + ")))
	# 
	# # 3. Fit model
	# #  i. Fixed 
	# m_fixed <- plm(m_eq, data=tmp, model="within")
	# 
	# #  ii. Random 
	# m_random <- plm(m_eq, data=tmp, model="random")
	
	# 4. Return results
	# return(list(m_fixed, m_random, m_eq))
	return(y)
}





