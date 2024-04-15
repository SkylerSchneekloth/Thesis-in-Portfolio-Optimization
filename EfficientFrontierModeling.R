rm(list = ls())
setwd("Path/To/Your/Working/Directory")

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

returnsType <- 2
backtest_window <- 60
periodLag <- 1
MaxConstraint <- 0.025
MinConstraint <- 0                  

#Load historical returns from working directory
benchmarks <- read.csv("benchmarks.csv",header=TRUE, row.names=1)

if (returnsType == 1) {
  
  #price returns
  returns <- read.csv("crsp_PriceReturnsFile.csv", header=TRUE, row.names=1)
  returns <- as.timeSeries(returns)
  benchmarks$vwretd <- NULL
  benchmarks$ewretd <- NULL
  print("you successfully imported asset-level PRICE returns from the crsp database, and deleted the dividend-INCLUSIVE benchmarks from your environment")
  
} else if (returnsType == 2) {
  
  #total return
  returns <- read.csv("crsp_TotalReturnsFile.csv", header=TRUE, row.names=1)
  returns <- as.timeSeries(returns)
  benchmarks$vwretx <- NULL
  benchmarks$ewretx <- NULL
  print("you successfully imported asset-level TOTAL returns from the crsp database, and deleted the divdend-EXCLUSIVE benchmarks from your environment")
  
} else {
  print("ERROR: change returnsType=1 for price return, or returnsType=2 for total return")
}

#Load S&P 500 constituent history from working directory
constituentHistory <- read.csv("siblis_ConstituentHistory.csv",header=TRUE)
constituentHistory <- as.data.frame(constituentHistory)
names(constituentHistory) <- sub('^X', '', names(constituentHistory))
names(constituentHistory) <- as.Date(names(constituentHistory), format="%Y.%m.%d")
constituentHistory[constituentHistory == ""] <- NA

#Lag to control for look-ahead bias
laggedReturns <- lag(returns, periodLag)
laggedReturns <- laggedReturns[-periodLag,]
returns <- returns[-periodLag,]
benchmarks <- benchmarks[-periodLag,]

#Define a ticker list for the most recent 5 years
FullHistoricalTickerList <- read.csv("FINAL_tickerList_unique_values.txt", header = FALSE)
roller <- backtest_window-1
i <- (nrow(returns)-roller)
tickerList <- na.exclude(constituentHistory[,roller+i])


#subset data
#Filter out non-tickerList stocks from return matrices
returns_tmp1 <- laggedReturns[, colnames(laggedReturns) %in% tickerList] #returns matrix to feed optimizer
returns_tmp2 <- returns[, colnames(returns) %in% as.vector(tickerList)] #returns matrix to calculate realized portfolio ret.

#Subset the return matrices to fit the rolling time frame
returns_tmp1 <- window(returns_tmp1,start = rownames(returns_tmp1)[i], end = rownames(returns_tmp1)[roller+i])
returns_tmp2 <- window(returns_tmp2,start = rownames(returns_tmp2)[i], end = rownames(returns_tmp2)[roller+i])

#Remove any stocks that do not have data for the full period
returns_tmp1 = returns_tmp1[, colSums(is.na(returns_tmp1)) == 0]; returns_tmp1 <- na.omit(returns_tmp1)
returns_tmp2 = returns_tmp2[, colSums(is.na(returns_tmp2)) == 0]; returns_tmp2 <- na.omit(returns_tmp2)
if (ncol(returns_tmp1)>ncol(returns_tmp2)) {
  returns_tmp1 <- returns_tmp1[, colnames(returns_tmp1) %in% colnames(returns_tmp2)]
} else if (ncol(returns_tmp1)<ncol(returns_tmp2)) { 
  returns_tmp2 <- returns_tmp2[, colnames(returns_tmp2) %in% colnames(returns_tmp1)]
}
returns_tmp2 <- returns_tmp2[backtest_window,]



###############################################################################
# Markowitz Portfolio Frontiers
###############################################################################

# Set baseline portfolio specifications
port_spec_base <- portfolio.spec(assets=colnames(returns_tmp1))
port_spec_base <- add.objective(portfolio = port_spec_base, type = 'return', name = 'mean')
port_spec_base <- add.constraint(portfolio = port_spec_base, type = 'weight_sum', min_sum = 0.99, max_sum = 1)
port_spec_base <- add.constraint(portfolio = port_spec_base, type = 'box', min = 0, max = 1)

# Generate optimal portfolios
max_weights <- c(0.5, 0.4, 0.3, 0.2, 0.1, 0.075, 0.05, 0.025)
portfolios <- list()
for (max_weight in max_weights) {
  port_spec <- port_spec_base
  port_spec <- add.constraint(portfolio = port_spec, type = 'box', min = 0, max = max_weight)
  portfolios[[paste0("MarkowitzPortfolio", which(max_weights == max_weight)-1)]] <- 
    optimize.portfolio(R = returns_tmp1, portfolio = port_spec, 
                       optimize_method = 'ROI', trace = TRUE)
}

# Accessing the results
portfolios$MarkowitzPortfolio0
portfolios$MarkowitzPortfolio1

ef0 <- extractEfficientFrontier(portfolios$MarkowitzPortfolio0) 
ef1 <- extractEfficientFrontier(portfolios$MarkowitzPortfolio1)
ef2 <- extractEfficientFrontier(portfolios$MarkowitzPortfolio2)
ef3 <- extractEfficientFrontier(portfolios$MarkowitzPortfolio3)
ef4 <- extractEfficientFrontier(portfolios$MarkowitzPortfolio4)
ef5 <- extractEfficientFrontier(portfolios$MarkowitzPortfolio5)
ef6 <- extractEfficientFrontier(portfolios$MarkowitzPortfolio6)
ef7 <- extractEfficientFrontier(portfolios$MarkowitzPortfolio7)

# Extract efficient frontiers
#indexNum <- seq_along(portfolios)
#frontiers <- list()
#for (i in indexNum) {
#  for (j in portfolios){
#    frontiers[[paste0("ef", which(indexNum == i)-1)]] <- extractEfficientFrontier(j)
#  }
#}

MVO_max_50 <- as.data.frame(ef0$frontier[,])[,1:2]
df1_max_40 <- as.data.frame(ef1$frontier[,])[,1:2]
df2_max_30 <- as.data.frame(ef2$frontier[,])[,1:2]
df3_max_20 <- as.data.frame(ef3$frontier[,])[,1:2]
df4_max_10 <- as.data.frame(ef4$frontier[,])[,1:2]
df5_max_7.5 <- as.data.frame(ef5$frontier[,])[,1:2]
df6_max_5 <- as.data.frame(ef6$frontier[,])[,1:2]
df7_max_2.5 <- as.data.frame(ef7$frontier[,])[,1:2]

#MVO_max_50 <- as.data.frame(frontiers$ef0$frontier[,])[,1:2]
#df1_max_40 <- as.data.frame(frontiers$ef1$frontier[,])[,1:2]
#df2_max_30 <- as.data.frame(frontiers$ef2$frontier[,])[,1:2]
#df3_max_20 <- as.data.frame(frontiers$ef3$frontier[,])[,1:2]
#df4_max_10 <- as.data.frame(frontiers$ef4$frontier[,])[,1:2]
#df5_max_7.5 <- as.data.frame(frontiers$ef5$frontier[,])[,1:2]
#df6_max_5 <- as.data.frame(frontiers$ef6$frontier[,])[,1:2]
#df7_max_2.5 <- as.data.frame(frontiers$ef7$frontier[,])[,1:2]

# Plot all results using ggplot2
library(dplyr)
library(purrr)
library(ggplot2)

data_frames <- list("Markowitz, max=50%" = MVO_max_50,
                    "Markowitz, max=40%" = df1_max_40,
                    "Markowitz, max=30%" = df2_max_30,
                    "Markowitz, max=20%" = df3_max_20,
                    "Markowitz, max=10%" = df4_max_10, 
                    "Markowitz, max=7.5%" = df5_max_7.5, 
                    "Markowitz, max=5%" = df6_max_5, 
                    "Markowitz, max=2.5%" = df7_max_2.5)

all_data <- bind_rows(data_frames, .id = "DataFrame")
all_data$DataFrame <- as.factor(all_data$DataFrame)

# Manually create a vector with the desired order of factor levels
levels_order <- c("Markowitz, max=50%", "Markowitz, max=40%", "Markowitz, max=30%", 
                  "Markowitz, max=20%", "Markowitz, max=10%", "Markowitz, max=7.5%", 
                  "Markowitz, max=5%", "Markowitz, max=2.5%")

# Convert the DataFrame column to a factor with the manually specified levels
all_data$DataFrame <- factor(all_data$DataFrame, levels = levels_order)

# Now create the ggplot
p <- ggplot(all_data, aes(x = ES, y = mean, group = DataFrame, color = DataFrame)) +
  geom_line() +
  labs(x = "Expected Shortfall", y = "Expected Return") +
  theme_minimal() +
  scale_color_viridis_d(name = "Portfolio", guide = guide_legend(title.position = "top")) +
  theme(legend.position = "right")

# Print the plot
print(p)



# MVO Portfolio #

port_spec <- portfolio.spec(assets=colnames(returns_tmp1))
port_spec <- add.objective(portfolio = port_spec, type = 'return', name = 'mean')
port_spec <- add.constraint(portfolio = port_spec, type = 'weight_sum', min_sum = 0.99, max_sum = 1)
port_spec <- add.constraint(portfolio = port_spec, type = 'box', min = MinConstraint, max = MaxConstraint)
port_spec = add.constraint(portfolio = port_spec, type = 'box', min = 0, max = 0.025)
opt_portfolio = optimize.portfolio(returns_tmp1,
                                   portfolio = port_spec,
                                   optimize_method = 'ROI',
                                   trace = TRUE)
efMarkowitz <- extractEfficientFrontier(opt_portfolio)
plot(efMarkowitz$frontier)
