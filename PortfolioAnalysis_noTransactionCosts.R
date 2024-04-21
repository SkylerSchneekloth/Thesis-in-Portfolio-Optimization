rm(list = ls())
setwd("C:/Users/Skyler/OneDrive - University of Iowa/FIN9391 Thesis in Equity Portfolio Optimization/RESULTS_no transaction costs") #setwd("Path/To/Your/Working/Directory")
benchmarkData <- read.csv("benchmarkData.csv",header=TRUE, row.names=1)
dfRAND <- read.csv("randomized_noTransactionCosts.csv",header=TRUE, row.names=1)
dfMVOa <- read.csv("MVO_unconstrained.csv",header=TRUE, row.names=1)
dfMVOb <- read.csv("MVO_TotRet_max2pt5.csv",header=TRUE, row.names=1)
dfMVOc <- read.csv("MVO_TotRet_max7pt5.csv",header=TRUE, row.names=1)
dfShrink0 <- read.csv("Shrink_25.csv",header=TRUE, row.names=1)
dfShrink1 <- read.csv("Shrink_50.csv",header=TRUE, row.names=1)
dfShrink2 <- read.csv("Shrink_75.csv",header=TRUE, row.names=1)
dfShrink3 <- read.csv("Shrink_100.csv",header=TRUE, row.names=1)
dfShrink4 <- read.csv("Shrink_250.csv",header=TRUE, row.names=1)
dfShrinkLW <- read.csv("LW.csv",header=TRUE, row.names=1)
dfREa <- read.csv("Resampled_50_mvMax2pt5.csv",header=TRUE, row.names=1)
dfREb <- read.csv("Resampled_100_mvMax2pt5.csv",header=TRUE, row.names=1)

library(quantmod)
library(timeSeries)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(fPortfolio)
library(caTools)
library(xts)
library(dplyr)
library(zoo)
library(tidyverse)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(corpcor)
library(pso)
library(GenSA)
library(ggplot2)
library(HDShOP)
library(corpcor)
library(MASS)
library(tseries)

LevelOfConfidence <- .7
rollingWindow <- 12

###############################################################################
# Risk Metrics
###############################################################################

#####
#Vol#
#####

SP500 <- sd(benchmarkData$vwretd, na.rm = TRUE)
RAND <- sd(dfRAND$portRet, na.rm = TRUE)
MVOa <- sd(dfMVOa$portRet, na.rm = TRUE)
MVOb <- sd(dfMVOb$portRet, na.rm = TRUE)
MVOc  <- sd(dfMVOc$portRet, na.rm = TRUE)
Shrink0 <- sd(dfShrink0$portRet, na.rm = TRUE)
Shrink1 <- sd(dfShrink1$portRet, na.rm = TRUE)
Shrink2 <- sd(dfShrink2$portRet, na.rm = TRUE)
Shrink3 <- sd(dfShrink3$portRet, na.rm = TRUE)
Shrink4 <- sd(dfShrink4$portRet, na.rm = TRUE)
ShrinkLW <- sd(dfShrinkLW$portRet, na.rm = TRUE)
REa <- sd(dfREa$portRet, na.rm = TRUE)
REb <- sd(dfREb$portRet, na.rm = TRUE)

#Original Vector as Data Frame
results_df <- data.frame(
  Method = c("row1", "row2", "row3", "row4", "row5", "row6", "row7", "row8", "row9", "row10", "row11", "row12", "row13"),
  col1 = c(SP500, RAND, MVOa, MVOb, MVOc, Shrink0, Shrink1, Shrink2, Shrink3, Shrink4, ShrinkLW, REa, REb))


#Vector Transformation
results_df_T <- results_df
results_df_T$col1 <- results_df_T$col1*sqrt(12)


#Display Results
print(data.frame(lapply(results_df, function(x) if(is.numeric(x)) round(x, 3) else x))) 
print(data.frame(lapply(results_df_T, function(x) if(is.numeric(x)) round(x, 3) else x)))



#####
#VaR#
#####

# Monthly VaR
calc_VaR <- function(x, prob=0.05, notional=1, min_obs=12) {
  non_na_data <- x[!is.na(x)]
  if (length(non_na_data) >= min_obs) {
    return(-quantile(non_na_data, prob) * notional)
  } else {
    return(NA)
  }
}

SP500 <- calc_VaR(benchmarkData$vwretd)
RAND <- calc_VaR(dfRAND$portRet)
MVOa <- calc_VaR(dfMVOa$portRet)
MVOb <- calc_VaR(dfMVOb$portRet)
MVOc  <- calc_VaR(dfMVOc$portRet)
Shrink0 <- calc_VaR(dfShrink0$portRet)
Shrink1 <- calc_VaR(dfShrink1$portRet)
Shrink2 <- calc_VaR(dfShrink2$portRet)
Shrink3 <- calc_VaR(dfShrink3$portRet)
Shrink4 <- calc_VaR(dfShrink4$portRet)
ShrinkLW <- calc_VaR(dfShrinkLW$portRet)
REa <- calc_VaR(dfREa$portRet)
REb <- calc_VaR(dfREb$portRet)

#Original Vector as Data Frame
results_df <- data.frame(
  Method = c("row1", "row2", "row3", "row4", "row5", "row6", "row7", "row8", "row9", "row10", "row11", "row12", "row13"),
  col1 = c(SP500, RAND, MVOa, MVOb, MVOc, Shrink0, Shrink1, Shrink2, Shrink3, Shrink4, ShrinkLW, REa, REb))


#Display Results
print(data.frame(lapply(results_df, function(x) if(is.numeric(x)) round(x, 3) else x))) 



######
#CVaR#
######

# Monthly CVaR
calc_CVaR <- function(x, prob=0.05, notional=1, min_obs=12) {
  VaR <- calc_VaR(x, prob, notional, min_obs)
  tail <- subset(x, x < -VaR)
  non_na_data <- x[!is.na(x)]
  if (length(non_na_data) >= min_obs) {
    return(-mean(unlist(tail)))
  } else {
    return(NA)
  }
}

SP500 <- calc_CVaR(benchmarkData$vwretd)
RAND <- calc_CVaR(dfRAND$portRet)
MVOa <- calc_CVaR(dfMVOa$portRet)
MVOb <- calc_CVaR(dfMVOb$portRet)
MVOc  <- calc_CVaR(dfMVOc$portRet)
Shrink0 <- calc_CVaR(dfShrink0$portRet)
Shrink1 <- calc_CVaR(dfShrink1$portRet)
Shrink2 <- calc_CVaR(dfShrink2$portRet)
Shrink3 <- calc_CVaR(dfShrink3$portRet)
Shrink4 <- calc_CVaR(dfShrink4$portRet)
ShrinkLW <- calc_CVaR(dfShrinkLW$portRet)
REa <- calc_CVaR(dfREa$portRet)
REb <- calc_CVaR(dfREb$portRet)

#Original Vector as Data Frame
results_df <- data.frame(
  Method = c("row1", "row2", "row3", "row4", "row5", "row6", "row7", "row8", "row9", "row10", "row11", "row12", "row13"),
  col1 = c(SP500, RAND, MVOa, MVOb, MVOc, Shrink0, Shrink1, Shrink2, Shrink3, Shrink4, ShrinkLW, REa, REb))


#Display Results
print(data.frame(lapply(results_df, function(x) if(is.numeric(x)) round(x, 3) else x))) 



##################
#Maximum Drawdown#
##################

maxdrawdown(benchmarkData$vwretd)[1]
maxdrawdown(dfRAND$portRet)[1]
maxdrawdown(dfMVOa$portRet)[1]
maxdrawdown(dfMVOb$portRet)[1]
maxdrawdown(dfMVOc$portRet)[1]
maxdrawdown(dfShrink0$portRet)[1]
maxdrawdown(dfShrink1$portRet)[1]
maxdrawdown(dfShrink2$portRet)[1]
maxdrawdown(dfShrink3$portRet)[1]
maxdrawdown(dfShrink4$portRet)[1]
maxdrawdown(dfShrinkLW$portRet)[1]
maxdrawdown(dfREa$portRet)[1]
maxdrawdown(dfREb$portRet)[1]


###############################################################################
# Other Metrics
###############################################################################

#####
#Min#
#####

SP500 <- min(benchmarkData$vwretd)
RAND <- min(dfRAND$portRet)
MVOa <- min(dfMVOa$portRet)
MVOb <- min(dfMVOb$portRet)
MVOc  <- min(dfMVOc$portRet)
Shrink0 <- min(dfShrink0$portRet)
Shrink1 <- min(dfShrink1$portRet)
Shrink2 <- min(dfShrink2$portRet)
Shrink3 <- min(dfShrink3$portRet)
Shrink4 <- min(dfShrink4$portRet)
ShrinkLW <- min(dfShrinkLW$portRet)
REa <- min(dfREa$portRet)
REb <- min(dfREb$portRet)

#Original Vector as Data Frame
results_df <- data.frame(
  Method = c("row1", "row2", "row3", "row4", "row5", "row6", "row7", "row8", "row9", "row10", "row11", "row12", "row13"),
  col1 = c(SP500, RAND, MVOa, MVOb, MVOc, Shrink0, Shrink1, Shrink2, Shrink3, Shrink4, ShrinkLW, REa, REb))


#Display Results
print(data.frame(lapply(results_df, function(x) if(is.numeric(x)) round(x, 3) else x))) 



#####
#Max#
#####

SP500 <- max(benchmarkData$vwretd)
RAND <- max(dfRAND$portRet)
MVOa <- max(dfMVOa$portRet)
MVOb <- max(dfMVOb$portRet)
MVOc  <- max(dfMVOc$portRet)
Shrink0 <- max(dfShrink0$portRet)
Shrink1 <- max(dfShrink1$portRet)
Shrink2 <- max(dfShrink2$portRet)
Shrink3 <- max(dfShrink3$portRet)
Shrink4 <- max(dfShrink4$portRet)
ShrinkLW <- max(dfShrinkLW$portRet)
REa <- max(dfREa$portRet)
REb <- max(dfREb$portRet)

#Original Vector as Data Frame
results_df <- data.frame(
  Method = c("row1", "row2", "row3", "row4", "row5", "row6", "row7", "row8", "row9", "row10", "row11", "row12", "row13"),
  col1 = c(SP500, RAND, MVOa, MVOb, MVOc, Shrink0, Shrink1, Shrink2, Shrink3, Shrink4, ShrinkLW, REa, REb))


#Display Results
print(data.frame(lapply(results_df, function(x) if(is.numeric(x)) round(x, 3) else x))) 



###########################
#Compounded Monthly Return#
###########################

N <- 12

SP500 <- (prod(1+benchmarkData$vwretd)^(1/N))-1
RAND <- (prod(1+dfRAND$portRet)^(1/N))-1
MVOa <- (prod(1+dfMVOa$portRet)^(1/N))-1
MVOb <- (prod(1+dfMVOb$portRet)^(1/N))-1
MVOc  <- (prod(1+dfMVOc$portRet)^(1/N))-1
Shrink0 <- (prod(1+dfShrink0$portRet)^(1/N))-1
Shrink1 <- (prod(1+dfShrink1$portRet)^(1/N))-1
Shrink2 <- (prod(1+dfShrink2$portRet)^(1/N))-1
Shrink3 <- (prod(1+dfShrink3$portRet)^(1/N))-1
Shrink4 <- (prod(1+dfShrink4$portRet)^(1/N))-1
ShrinkLW <- (prod(1+dfShrinkLW$portRet)^(1/N))-1
REa <- (prod(1+dfREa$portRet)^(1/N))-1
REb <- (prod(1+dfREb$portRet)^(1/N))-1

#Original Vector as Data Frame
results_df <- data.frame(
  Method = c("row1", "row2", "row3", "row4", "row5", "row6", "row7", "row8", "row9", "row10", "row11", "row12", "row13"),
  col1 = c(SP500, RAND, MVOa, MVOb, MVOc, Shrink0, Shrink1, Shrink2, Shrink3, Shrink4, ShrinkLW, REa, REb))


#Display Results
print(data.frame(lapply(results_df, function(x) if(is.numeric(x)) round(x, 3) else x))) 



#################
#Terminal Wealth#
#################

benchmarkData$wealth_index[nrow(benchmarkData)]
dfRAND$wealth_index[nrow(benchmarkData)]
dfMVOa$wealth_index[nrow(benchmarkData)]
dfMVOb$wealth_index[nrow(benchmarkData)]
dfMVOc$wealth_index[nrow(benchmarkData)]
dfShrink0$wealth_index[nrow(benchmarkData)]
dfShrink1$wealth_index[nrow(benchmarkData)]
dfShrink2$wealth_index[nrow(benchmarkData)]
dfShrink3$wealth_index[nrow(benchmarkData)]
dfShrink4$wealth_index[nrow(benchmarkData)]
dfShrinkLW$wealth_index[nrow(benchmarkData)]
dfREa$wealth_index[nrow(benchmarkData)]
dfREb$wealth_index[nrow(benchmarkData)]


###############################################################################
# Chart Formatting
###############################################################################

# Assuming wealth_index_ts is a ts object with monthly data
# Set up custom colors with transparency (alpha value)
colors <- c(rgb(1, 0, 0),        # Red
            rgb(1, 0, 0, 0.3),   # Red with 70% transparency
            rgb(0, 0, 0),        # Black
            rgb(0, 0, 1, 0.3),   # Blue with 70% transparency
            rgb(0, 0, 0),        # Black
            rgb(0, 0, 0)         # Black
)

# Set up line widths, one for each series
line_widths <- c(3, 
                 3, 
                 3, 
                 3,
                 1.5,
                 1.5)

# Set up line types
line_types <- c("solid", 
                "solid", 
                "solid", 
                "solid",
                "solid",
                "dotted")

###############################################################################
# Wealth Index
###############################################################################

wealth_index_data <- cbind(benchmarkData$wealth_index,
                           dfRAND$wealth_index,
                           dfMVOb$wealth_index,
                           dfREb$wealth_index,
                           dfShrink4$wealth_index, 
                           dfShrinkLW$wealth_index
)

colnames(wealth_index_data) <- c("S&P 500", 
                                 "randomized",
                                 "MVO",
                                 "RE",
                                 "Bodnar shrinkage",
                                 "Ledoit-Wolf shrinkage"
)

wealth_index_ts <- ts(wealth_index_data, start = c(1994, 12), end = c(2023, 12), frequency = 12)

# Plotting the time series
plot(wealth_index_ts, plot.type = "single", col = colors, lwd = line_widths, lty = line_types,
     xlab = "Time", ylab = "Wealth Index")

# Add legend
legend("topleft", legend = colnames(wealth_index_ts), col = colors, lty = line_types, lwd = line_widths)




###############################################################################
# Drawdowns
###############################################################################

drawdowns_data <- cbind(benchmarkData$drawdown,
                        dfRAND$drawdown,
                        dfMVOb$drawdown,
                        dfREb$drawdown,
                        dfShrink4$drawdown, 
                        dfShrinkLW$drawdown
)

colnames(drawdowns_data) <- c("S&P 500", 
                              "randomized",
                              "MVO",
                              "RE",
                              "Bodnar shrinkage",
                              "Ledoit-Wolf shrinkage"
)

drawdowns_data_ts <- ts(drawdowns_data, start = c(1994, 12), end = c(2023, 12), frequency = 12)

# Plotting the time series
plot(drawdowns_data_ts, plot.type = "single", col = colors, lwd = line_widths, lty = line_types,
     xlab = "Time", ylab = "Wealth Index")

# Add legend
legend("bottomleft", legend = colnames(drawdowns_data_ts), col = colors, lty = line_types, lwd = line_widths)

###############################################################################
# Gamma Chart
###############################################################################

gamma_data <- read.csv("gamma_vs_terminal_wealth.csv", header = TRUE)

plot(x = gamma_data$gamma,
     y = gamma_data$terminalWealth,
     xlab = "Gamma", 
     ylab = "Terminal Wealth",
     cex.axis = 1.5,   # Increases the size of axis text
     cex.lab = 1.5,    # Increases the size of x and y labels
     cex.main = 1.5)   # Optionally, increase the size of the main title (if added)




###############################################################################
# T-stat Chart comparing Risk-Adjusted Returns
###############################################################################

library(zoo)
library(dplyr)
library(ggplot2)
library(tibble)

benchmarkData <- rownames_to_column(benchmarkData, var = "date")
benchmarkData$date <- as.Date(benchmarkData$date, format = "%Y-%m-%d")


# Combine the two series into one data frame
combined_data <- data.frame(
  date = benchmarkData$date,
  vwretd = benchmarkData$vwretd,
  portRet = dfMVOb$portRet
)

# Ensure that 'date' column is of Date type
combined_data$date <- as.Date(combined_data$date, format = "%Y-%m-%d")

# Rolling t-test function
roll_t_test <- function(x, y) {
  t_result <- t.test(x, y)
  return(t_result$statistic)
}

# Apply the rolling t-test
results <- rollapply(data = combined_data[, c("vwretd", "portRet")],
                     width = rollingWindow,
                     FUN = function(x) roll_t_test(x[, 1], x[, 2]),
                     by.column = FALSE,
                     align = 'right')

# Combine the results with the dates
results <- data.frame(date = combined_data$date[(rollingWindow:length(combined_data$date))], 
                      t_statistic = as.vector(results))

# Convert to tibble for easier subsetting and plotting
results <- as_tibble(results)

# Plot for the first time period (1995-12-30 to 2013-12-30)
first_period <- results %>% 
  filter(date >= as.Date("1995-12-30") & date <= as.Date("2013-12-30"))

ggplot(first_period, aes(x = date, y = t_statistic)) +
  geom_line() +
  labs(x = "Date", y = "T-Statistic", title = "Rolling T-Test of Stock Returns: 1995-2013") +
  theme_minimal()

# Plot for the second time period (2014-01-31 to 2023-12-30)
second_period <- results %>% 
  filter(date >= as.Date("2014-01-31") & date <= as.Date("2023-12-30"))

ggplot(second_period, aes(x = date, y = t_statistic)) +
  geom_line() +
  labs(x = "Date", y = "T-Statistic", title = "Rolling T-Test of Stock Returns: 2014-2023") +
  theme_minimal()

# Plot for the entire time period
ggplot(results, aes(x = date, y = t_statistic)) +
  geom_line() +
  labs(x = "Date", y = "T-Statistic", title = "Rolling T-Test of Stock Returns: Entire Data Range") +
  theme_minimal()

# Bar charts instead of line charts including 95% confidence interval

z <- qt(p = (1 - LevelOfConfidence)/2, df = Inf, lower.tail = FALSE)

# first period
ggplot(first_period, aes(x = date, y = t_statistic)) +
  geom_bar(stat="identity", fill="blue") +
  geom_hline(yintercept = c(-z, z), linetype="dashed", color="red", size=1) +
  labs(x = "Date", y = "T-Statistic") +
  theme_minimal()

# second period
ggplot(second_period, aes(x = date, y = t_statistic)) +
  geom_bar(stat="identity", fill="blue") +
  geom_hline(yintercept = c(-z, z), linetype="dashed", color="red", size=1) +
  labs(x = "Date", y = "T-Statistic") +
  theme_minimal()

# full period
ggplot(results, aes(x = date, y = t_statistic)) +
  geom_bar(stat="identity", fill="blue") +
  geom_hline(yintercept = c(-z, z), linetype="dashed", color="red", size=1) +
  labs(x = "Date", y = "T-Statistic") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 18), # Increase size of x-axis title
    axis.title.y = element_text(size = 18), # Increase size of y-axis title
    axis.text.x = element_text(size = 18),  # Increase size of x-axis text
    axis.text.y = element_text(size = 18)   # Increase size of y-axis text
  )

