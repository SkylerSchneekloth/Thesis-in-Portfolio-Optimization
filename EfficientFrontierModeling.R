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
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )

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


###############################################################################
# Frontier Portfolio Composition Map: Markowitz
###############################################################################


allocations <- rbind(
  portfolios$MarkowitzPortfolio0$weights,
  portfolios$MarkowitzPortfolio1$weights,
  portfolios$MarkowitzPortfolio2$weights,
  portfolios$MarkowitzPortfolio3$weights,
  portfolios$MarkowitzPortfolio4$weights,
  portfolios$MarkowitzPortfolio5$weights,
  portfolios$MarkowitzPortfolio6$weights,
  portfolios$MarkowitzPortfolio7$weights
)

# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Identify non-zero columns
non_zero_cols <- colSums(allocations) > 0

# Filter data to only non-zero columns
filtered_allocations <- allocations[, non_zero_cols]

# Convert the matrix to a data frame
allocations_df <- as.data.frame(filtered_allocations)

# Add a row index to the data frame
allocations_df$Row <- seq_len(nrow(allocations_df))

# Melt the data frame to long format
long_allocations_df <- pivot_longer(allocations_df, cols = -Row, names_to = "Ticker", values_to = "Value")


# Define custom x-axis labels
x_labels <- c(
  "1" = "50% max alloc.",
  "2" = "40% max alloc.",
  "3" = "30% max alloc.",
  "4" = "20% max alloc.",
  "5" = "10% max alloc.",
  "6" = "7.5% max alloc.",
  "7" = "5% max alloc.",
  "8" = "2.5% max alloc."
)

# Plot using ggplot
p <- ggplot(long_allocations_df, aes(x = factor(Row), y = Value, fill = Ticker)) +
  geom_bar(stat = "identity", color = "black") + # Add thin black lines
  scale_fill_manual(values = rainbow(length(unique(long_allocations_df$Ticker)))) +
  scale_x_discrete(labels = x_labels) + # Custom x-axis labels
  ylab(NULL) + # Remove y-axis label
  xlab(NULL) + # Remove x-axis label; optional if you want to have a label
  guides(fill=guide_legend(ncol=1)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14), # Rotate x labels and increase size
    axis.text.y = element_text(size = 14) # Increase y-axis text size
  ) +
  theme (
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )

# Print the plot
print(p)


###############################################################################
# Frontier Portfolio Composition Map: Resampling
###############################################################################

# Load optimal weights
setwd("Path/To/No/Transaction/Costs/Data/Repository")

A <- read.csv("Resampled_100_mvMax0.5_weights.csv",header=TRUE, row.names=1)
B <- read.csv("Resampled_100_mvMax0.4_weights.csv",header=TRUE, row.names=1)
C <- read.csv("Resampled_100_mvMax0.3_weights.csv",header=TRUE, row.names=1)
D <- read.csv("Resampled_100_mvMax0.2_weights.csv",header=TRUE, row.names=1)
E <- read.csv("Resampled_100_mvMax0.1_weights.csv",header=TRUE, row.names=1)
F <- read.csv("Resampled_100_mvMax0.075_weights.csv",header=TRUE, row.names=1)
G <- read.csv("Resampled_100_mvMax0.05_weights.csv",header=TRUE, row.names=1)
H <- read.csv("Resampled_100_mvMax2pt5_weights.csv",header=TRUE, row.names=1)

A <- A[nrow(A),]  # Select the most recent observation
B <- B[nrow(B),]  # Select the most recent observation
C <- C[nrow(C),]  # Select the most recent observation
D <- D[nrow(D),]  # Select the most recent observation
E <- E[nrow(E),]  # Select the most recent observation
F <- F[nrow(F),]  # Select the most recent observation
G <- F[nrow(F),]  # Select the most recent observation
H <- F[nrow(F),]  # Select the most recent observation

A <- A[, !apply(A, 2, function(x) any(is.na(x) | x == 0))] # Remove columns that contain any NAs or zeros
B <- B[, !apply(B, 2, function(x) any(is.na(x) | x == 0))] # Remove columns that contain any NAs or zeros
C <- C[, !apply(C, 2, function(x) any(is.na(x) | x == 0))] # Remove columns that contain any NAs or zeros
D <- D[, !apply(D, 2, function(x) any(is.na(x) | x == 0))] # Remove columns that contain any NAs or zeros
E <- E[, !apply(E, 2, function(x) any(is.na(x) | x == 0))] # Remove columns that contain any NAs or zeros
F <- F[, !apply(F, 2, function(x) any(is.na(x) | x == 0))] # Remove columns that contain any NAs or zeros
G <- G[, !apply(G, 2, function(x) any(is.na(x) | x == 0))] # Remove columns that contain any NAs or zeros
H <- H[, !apply(H, 2, function(x) any(is.na(x) | x == 0))] # Remove columns that contain any NAs or zeros

data_frames <- list("50% max alloc." = A,
                    "40% max alloc." = B,
                    "30% max alloc." = C,
                    "20% max alloc." = D,
                    "10% max alloc." = E,
                    "7.5% max alloc." = F,
                    "5% max alloc." = G,
                    "2.5% max alloc." = H)

# Bind rows and set 'REport' as an id
all_data <- bind_rows(data_frames, .id = "REport")
all_data$REport <- factor(all_data$REport, levels = c("50% max alloc.", 
                                                      "40% max alloc.",
                                                      "30% max alloc.", 
                                                      "20% max alloc.", 
                                                      "10% max alloc.",
                                                      "7.5% max alloc.",
                                                      "5% max alloc.",
                                                      "2.5% max alloc."))

# Melt the data frame to long format
long_all_data <- all_data %>% 
  pivot_longer(cols = -REport, names_to = "Ticker", values_to = "Weight")

# Plot using ggplot
p <- ggplot(long_all_data, aes(x = REport, y = Weight, fill = Ticker)) +
  geom_bar(stat = "identity", color = "black") + # Add thin black lines
  scale_fill_manual(values = rainbow(length(unique(long_all_data$Ticker)))) +
  xlab("Portfolio") + # Optionally add x-axis label
  theme_minimal() +
  theme(
    legend.position = "none", # Hide legend
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14), # Rotate x labels and increase size
    axis.text.y = element_text(size = 14), # Increase y-axis text size
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )

# Print the plot
print(p)
