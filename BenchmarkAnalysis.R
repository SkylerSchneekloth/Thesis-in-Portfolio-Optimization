rm(list = ls())
setwd("C:/Users/Skyler/OneDrive - University of Iowa/FIN9391 Thesis in Equity Portfolio Optimization")
df <- read.csv("benchmarks_truncated.csv",header=TRUE, row.names=1)

portRet <- df$vwretd
analysis_window <- 12
CVaRpercentile <- 0.05


#COMPILE RESULTS##################################################################################################################################

# Let's start by calculating actual compounded annual portfolio return
calc_CAR <- function(ret){
  car <- prod(1+ret)-1 # Assuming x is simple return, add 1 for cumulative product calculation
  return(car)
}
df["CompoundAnnualReturn"] <- rollapply(portRet, width=analysis_window, FUN = calc_CAR, fill=NA, align="right")

## At this point, we have two single-column vectors for portfolio returns, one monthly and one compounded annual.
## Now we can calculate portfolio analytics based on either single-column vector

#########################################################################################
#################################### M O N T H L Y ###################################### 
#########################################################################################
#The following analysis is executed over the single-column vector of MONTHLY portfolio-level returns


# Min/Max Monthly Return
df["Min_monthly"] <- rollapply(portRet, width=analysis_window, FUN = min, fill=NA, align="right")
df["Max_monthly"] <- rollapply(portRet, width=analysis_window, FUN = max, fill=NA, align="right")


# Monthly Standard Deviation
df["portStdDev_monthly"] <- rollapply(portRet, width=analysis_window, FUN = sd, fill=NA, align="right")


# Monthly VaR
calc_VaR <- function(x, prob=CVaRpercentile, notional=1, min_obs=analysis_window) {
  non_na_data <- x[!is.na(x)]
  if (length(non_na_data) >= min_obs) {
    return(-quantile(non_na_data, prob) * notional)
  } else {
    return(NA)
  }
}
df["portVaR"] <- rollapply(portRet, width=analysis_window, FUN = calc_VaR, fill=NA, align="right")


# Monthly CVaR
calc_CVaR <- function(x, prob=CVaRpercentile, notional=1, min_obs=analysis_window) {
  VaR <- calc_VaR(x, prob, notional, min_obs)
  tail <- subset(x, x < -VaR)
  non_na_data <- x[!is.na(x)]
  if (length(non_na_data) >= min_obs) {
    return(-mean(unlist(tail)))
  } else {
    return(NA)
  }
}
df["portCVaR"] <-rollapply(portRet, width=analysis_window, FUN = calc_CVaR, fill=NA, align="right")


# Wealth Index
df <- df %>% mutate(growth_multiplier = 1 + portRet)
df[1, "growth_multiplier"] <- 1 #start with $1000
df <- df %>% mutate(wealth_index = cumprod(growth_multiplier) * 1000)


# Drawdowns

# Dollars
df$peak_wealth_index <- cummax(df$wealth_index)
df$drawdown <- df$wealth_index - df$peak_wealth_index
max_drawdown <- min(df$drawdown)

# Percent
df$drawdown_pct <- (df$wealth_index - df$peak_wealth_index) / df$peak_wealth_index
max_drawdown_pct <- min(df$drawdown_pct)
      
      
write.csv(df, "benchmarkData.csv")