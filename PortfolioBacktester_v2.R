#CLEAR WORKSPACE, SET WORKING DIRECTORY##############################################################################################################
#It is always best practice to first clear the RStudio environment

rm(list = ls())
setwd("C:/Users/Skyler/OneDrive - University of Iowa/FIN9391 Thesis in Equity Portfolio Optimization")



#SET PARAMETERS######################################################################################################################################
#User settings to calibrate the back-test and name file output

filename <- "Test"

#VARY THESE PARAMETERS TO PERFORM BACKTESTS:

  optimizer <- 6                      #1 Random weight w/ (LONG ONLY) w/ base R
                                      #2 MV unconstrained (LONG ONLY) w/ package=PortfolioAnalytics
                                      #3 MV with constraints (LONG ONLY) w/ package=PortfolioAnalytics
                                      #4 Bodnar-style shrinkage method (LONG/SHORT) w/ package=HDShOP
                                      #5 Ledoit-Wolf style shrinkage method (LONG/SHORT) w/ the HDShOP and tseries packages
                                      #6 Michaud-style bootstrap aka resampling method (LONG/SHORT) w/ base R and the PortfolioAnalytics package
  
  returnsType <- 2                    #1 load CRSP price return only (RETX)
                                      #2 load CRSP total return (RET)
  
  transaction_costs <- 'on'           #'on' or 'off' ('on' applies the "fees" number below as the marginal transaction cost of portfolio rebalancing)
  
  MaxConstraint <- 0.025              #The maximum asset allocation per stock for MVO (some values may cause errors, if this happens simply relax the constraint and try again)
  MinConstraint <- 0                  #The minimum asset allocation per stock for MVO (some values may cause errors, if this happens simply relax the constraint and try again)
  gamma <- 250                         #The "coefficient of risk aversion" for shrinkage portfolio
  n_bootstraps <- 10                  #The number of bootstraps to resampled portfolio


  
#LEAVE THESE PARAMETERS UNCHANGED:
# These parameters are coded in already (i.e., the R program will run successfully if you change them) but we're not really interested in testing variations in these parameters at the moment.
# Testing how robust our results are to changes in some of these parameters might be good for "future research".

  backtest_window <- 60               #Choose a rolling time frame for the back-test
  analysis_window <- 12               #A series of monthly time intervals adding up to the total performance sample under consideration (see lines 213:end)
  CVaRpercentile <- 0.05              #Desired CVaR percentile
  periodLag <- 1                      #0 assume perfect information
                                      #1 assume non-perfect information (controls for look-ahead bias)
  MarketBenchmarkWeighting <- 1       #1 Value-Weighted S&P 500 Index
                                      #2 Equal-Weighted S&P 500 Index
  fees <- 0.0025                      # set equal to marginal transaction cost
  max_random_turnover <- .2           # the rebalancing constraint on a random portfolio when optimizer==1
  annualizeRiskAdjRet <- 'no'         # do you want to report annualized monthly SR, IR, and TR? "yes" or "no"



#IDEAS FOR ADDITIONAL PARAMETERS:
# Having these options would be great, but they are not coded yet. Consider these "nice-to-haves" / saving for "future research".
# Would be interesting to test how robust our results are to changes in how asset-level expected returns are initially calculated.
# Note that missing values are not an issue with the CRSP data, but might be for other data sources (in which case it may be convenient to have some built-in data pre-processing).

#typeOfExpectedReturns <- 1         #1 long-run average of historical returns
                                     #2 [idea] single-factor CAPM
                                     #3 [idea] 3-factor Fama-French
                                     #4 [idea] 3-factor Fama-French plus momentum (Carhart's Four Factor Model)
                                     #5 [idea] Goyal and Welch (2008)
                                     #6 [idea] Monte Carlo simulation (assuming lognormally-distributed returns) combined with a lattice model such as a multi-period binomial tree
                                     #7 [idea] Monte Carlo simulation (assuming poisson distribution) combined with a lattice model such as a multi-period binomial tree
                                     #8 [idea] Geometric Brownian Motion
                                     #9 [idea] Machine Learning algorithms other than regression analysis (e.g., Neural Networks)
                                     #10 [idea] ARIMA and/or GARCH-style models
                                     #11 [idea] Finite Difference Models / Jump Diffusion (both stable and unstable jump diffusion)

#HandleMissingValues <- 1           #How do you want to handle missing elements in the asset-level return streams? This will specifically affect iterations of our back-test lacking a full price history across the entire S&P 500 ticker list.
                                    #1 delete all assets w/o a full returns history from the ticker list
                                    #2 [idea] carry-forward last known observation (copy+paste)
                                    #3 [idea] impute i.e., replace with an expected value



#PREPARE WORKSPACE###################################################################################################################################
#These packages are necessary to execute the RStudio program

#install.packages("quantmod")
#install.packages("timeSeries")
#install.packages("PerformanceAnalytics")
#install.packages("PortfolioAnalytics")
#install.packages("fPortfolio")
#install.packages("caTools")
#install.packages("xts")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("ROI")
#install.packages("corpcor")
#install.packages("pso")
#install.packages("GenSA")
#install.packages("ggplot2)
#install.packages("HDShOP")
#install.packages("corpcor")
#install.packages("MASS")
#install.packages("tseries")
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

options(scipen=999)  #999 turns off scientific notation, scipen=0 turns it back on



#READ DATA###########################################################################################################################################
#Here, we draw from a local file system with each .csv document representing a standard two-dimensional data table

#Load benchmarks from working directory
benchmarks <- read.csv("benchmarks.csv",header=TRUE, row.names=1)

#Load S&P 500 constituent history from working directory
constituentHistory <- read.csv("siblis_ConstituentHistory.csv",header=TRUE)
constituentHistory <- as.data.frame(constituentHistory)
names(constituentHistory) <- sub('^X', '', names(constituentHistory))
names(constituentHistory) <- as.Date(names(constituentHistory), format="%Y.%m.%d")
constituentHistory[constituentHistory == ""] <- NA

#Load historical returns from working directory
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



#APPLY LAGS#####################################################################################################################################
#the purpose of lagging by one period is to obtain the portfolio weights that you WOULD HAVE used going into the next period 

laggedReturns <- lag(returns, periodLag)
laggedReturns <- laggedReturns[-periodLag,]
returns <- returns[-periodLag,]
benchmarks <- benchmarks[-periodLag,]

#LOOP###########################################################################################################################################

# Truncate benchmarks data frame to fit backtest
benchmarks_truncated <- benchmarks[backtest_window:nrow(benchmarks),]
write.csv(benchmarks_truncated,"benchmarks_truncated.csv")

#This is a convenience variable:
roller <- backtest_window-1

#Initialize an object for collecting monthly portfolio-level returns
df <- data.frame("portRet"=rep(NA, nrow(returns)-roller), row.names = rownames(benchmarks_truncated))
#df["turnover"] <- rep(NA, nrow(df))

#Initialize an object for collecting asset weights
FullHistoricalTickerList <- read.csv("FINAL_tickerList_unique_values.txt", header = FALSE)
df_weights_bgn <- matrix(data=NA, nrow=nrow(benchmarks_truncated), ncol=nrow(FullHistoricalTickerList))
df_weights_bgn <- data.frame(df_weights_bgn, row.names=rownames(benchmarks_truncated))
colnames(df_weights_bgn) <- FullHistoricalTickerList$V1
df_weights_end <- matrix(data=NA, nrow=nrow(benchmarks_truncated), ncol=nrow(FullHistoricalTickerList))
df_weights_end <- data.frame(df_weights_end, row.names=rownames(benchmarks_truncated))
colnames(df_weights_end) <- FullHistoricalTickerList$V1

#Here is the for loop that executes our backtest:
for (i in 1:(nrow(returns)-roller)){
  
  #Set Ticker List
  tickerList <- na.exclude(constituentHistory[,roller+i])
  #names(constituentHistory)[60]
  
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
  
  
  # run optimizers; find weights
  if (optimizer==1) {
    
    #random
    
      set.seed(i) # For reproducibility
      n <- ncol(returns_tmp1)
      randomNumbers <- runif(n)
      normalized_scaled_values <- randomNumbers / sum(randomNumbers)
      random_portfolio <- normalized_scaled_values
      names(random_portfolio) <- names(returns_tmp1)
    
  } else if (optimizer==2) {
    
    #MVO long only
    
      port_spec = portfolio.spec(colnames(returns_tmp1))
      port_spec = add.objective(portfolio = port_spec, type = 'return', name = 'mean')
      port_spec = add.constraint(portfolio = port_spec, type = 'weight_sum', min_sum = .99, max_sum = 1)
      #if (transaction_costs=='on') {portData  <- returns_tmp1-fees} else {portData  <- returns_tmp1}
      #if (transaction_costs=='on') {port_spec = add.constraint(portfolio=port_spec, type="transaction_cost", ptc=fees)}
      opt_portfolio = optimize.portfolio(returns_tmp1,
                                         portfolio = port_spec,
                                         optimize_method = 'ROI',
                                         trace = TRUE)
    
  } else if (optimizer==3) {
    
    #MVO w/ constraints
    
      port_spec = portfolio.spec(colnames(returns_tmp1))
      port_spec = add.objective(portfolio = port_spec, type = 'return', name = 'mean')
      port_spec = add.constraint(portfolio = port_spec, type = 'weight_sum', min_sum = .99, max_sum = 1)
      port_spec = add.constraint(portfolio = port_spec, type = 'box', min = MinConstraint, max = MaxConstraint)
      #if (transaction_costs=='on') {portData  <- returns_tmp1-fees} else {portData  <- returns_tmp1}
      #if (transaction_costs=='on') {port_spec = add.constraint(portfolio=port_spec, type="transaction_cost", ptc=fees)}
      opt_portfolio = optimize.portfolio(returns_tmp1,
                                   portfolio = port_spec,
                                   optimize_method = 'ROI',
                                   trace = TRUE)
    
  } else if (optimizer==4) {
  
    # Shrinkage method 1 w/ HDsHOP optimizer

      ret2 <- as.data.frame(returns_tmp1)                     #convert to dataframe for HDShOP
      ret3 <- t(ret2)                                         #transpose for HDShOP
      portData  <- as.matrix(ret3)
      p <-nrow(portData)    # number of assets
      b <-rep(1/p,p)        # setup matrix for optimizer
      opt_portfolio <- MVShrinkPortfolio(x=portData, gamma=gamma, type="shrinkage", b=b, beta=0.05) #x = p by n matrix or a data frame of asset returns; rows represent different assets, columns – observations.
                                                                                                    #gamma = 	numeric variable; coefficient of risk aversion.
                                                                                                    #b = numeric variable; the weights of the target portfolio.
                                                                                                    #beta = numeric variable; the confidence level for weight intervals.
      
  } else if (optimizer==5) {
    
    # Shrinkage method 2 w/ tseries optimizer
    
      # Use HDsHOP to obtain the optimally shrunk covariance matrix from Ledoit-Wolf (2020) 
      ret2 <- as.data.frame(returns_tmp1)                     #convert to dataframe for HDShOP
      ret3 <- t(ret2)    
      ShrunkCovarianceMatrix <- CovarEstim(ret3, type="LW20")
      
      # Pass the shrunk covariance matrix to tseries optimizer 
      opt_portfolio <- portfolio.optim(x=returns_tmp1,
                                       shorts = TRUE,
                                       reslow = rep(MinConstraint, ncol(returns_tmp1)),
                                       reshigh = rep(MaxConstraint, ncol(returns_tmp1)),
                                       covmat = ShrunkCovarianceMatrix
                                       )
  
  } else if (optimizer==6) {
    
    # Resampling method
  
    #STEP 3# Repeat steps 1 and 2 (until enough observations are available for convergence in step 4)
    
    re_container <- matrix(nrow = n_bootstraps, ncol = ncol(returns_tmp1))
    re_container <- as.data.frame(re_container)
    names(re_container) <- names(returns_tmp1)
    
    
    original_means <- colMeans(returns_tmp1)
    original_cov <- cov(returns_tmp1) #sample covariance matrix
    
    for (k in 1:n_bootstraps) {
      
      #STEP 1# Sample a mean vector and covariance matrix (of returns) from a distribution of both centered at the original (point estimate) values normally used in MV optimization
        random_returns <- mvrnorm(n=backtest_window, mu = original_means, Sigma = original_cov)
        random_returns <- as.timeSeries(random_returns)
        rownames(random_returns) <- rownames(returns_tmp1)
      
      
      #STEP 2# Calculate a MV efficient frontier based on these sampled risk and return estimates
        port_spec = portfolio.spec(colnames(random_returns))
        port_spec = add.objective(portfolio = port_spec, type = 'return', name = 'mean')
        port_spec = add.constraint(portfolio = port_spec, type = 'weight_sum', min_sum = .99, max_sum = 1)
        #port_spec = add.constraint(portfolio = port_spec, type = 'box', min = 0, max = 1)
        port_spec = add.constraint(portfolio = port_spec, type = 'box', min = MinConstraint, max = MaxConstraint)
        #if (transaction_costs=='on') {portData  <- returns_tmp1-fees} else {portData  <- returns_tmp1}  #minus 50 basis points from expected returns to account for transaction costs
        #if (transaction_costs=='on') {port_spec = add.constraint(portfolio=port_spec, type="transaction_cost", ptc=fees)}
        opt_portfolio = optimize.portfolio(random_returns,
                                           portfolio = port_spec,
                                           optimize_method = 'ROI',
                                           trace = TRUE)
        
        re_container[k, names(opt_portfolio$weights)] <- opt_portfolio$weights #similar to HLOOKUP in Microsoft Excel 
        
    }
      
    #STEP 4# Average the portfolio weight from step 2 to form the RE optimal portfolio
    
    re_weights <- colMeans(re_container)
    
    
  } else {
    
    print("message")
    
  }
  

  # capture asset weights and calculate monthly portfolio returns
  if (optimizer==1) {
    df_weights_bgn[i, names(random_portfolio)] <- random_portfolio #similar to HLOOKUP in Microsoft Excel
    df_weights_end[i, names(random_portfolio)] <- (as.vector(random_portfolio)*as.vector(1+returns_tmp2))
    portRet <- sum(as.vector(random_portfolio)*as.vector(returns_tmp2))
  } else if (optimizer==2 | optimizer==3) {
    df_weights_bgn[i, names(opt_portfolio$weights)] <- opt_portfolio$weights #similar to HLOOKUP in Microsoft Excel
    df_weights_end[i, names(opt_portfolio$weights)] <- (as.vector(opt_portfolio$weights)*as.vector(1+returns_tmp2))
    portRet <- sum(as.vector(opt_portfolio$weights)*as.vector(returns_tmp2))
  } else if (optimizer==4) {
    AssetWeights <- as.data.frame(opt_portfolio$weights, row.names = row.names(ret3))
    df_weights_bgn[i, rownames(AssetWeights)] <- AssetWeights[,1] #similar to HLOOKUP in Microsoft Excel
    df_weights_end[i, names(AssetWeights)] <- (AssetWeights*as.vector(1+returns_tmp2))
    portRet <- sum(as.vector(opt_portfolio$weights)*as.vector(returns_tmp2))
  } else if (optimizer==5) {
    AssetWeights <- as.data.frame(opt_portfolio$pw, row.names = row.names(ret3))
    df_weights_bgn[i, rownames(AssetWeights)] <- AssetWeights[,1] #similar to HLOOKUP in Microsoft Excel
    df_weights_end[i, names(AssetWeights)] <- (AssetWeights*as.vector(1+returns_tmp2))
    portRet <- sum(as.vector(opt_portfolio$pw)*as.vector(returns_tmp2))
  } else if (optimizer==6) {
    AssetWeights <- as.data.frame(re_weights, row.names = names(re_weights))
    df_weights_bgn[i, rownames(AssetWeights)] <- AssetWeights[,1] #similar to HLOOKUP in Microsoft Excel
    df_weights_end[i, names(AssetWeights)] <- (AssetWeights*as.vector(1+returns_tmp2))
    portRet <- sum(AssetWeights*as.vector(returns_tmp2))
  }
  
  df[i, 1] <- portRet
 
}


# Calculate portfolio turnover from monthly rebalancing as sum of change in asset weights MoM
row_sums <- rowSums(df_weights_end, na.rm = TRUE)
df_weights_end_normalized <- df_weights_end/row_sums
monthEndPositions <- df_weights_end_normalized
startingPositions <- as.data.frame(lag(as.timeSeries(monthEndPositions),1))[-1,]
newOptimalPositions <- df_weights_bgn[-1,]
rebalancingAdjustments = newOptimalPositions-startingPositions
    df = as.data.frame(df[-1,]); names(df) = "portRet"
    benchmarks_truncated = benchmarks_truncated[-1,]
df["turnover"] <- rowSums(abs(rebalancingAdjustments), na.rm = TRUE)/2

# Apply proportional transaction costs to monthly portfolio returns.
if (transaction_costs=='on') {
  df$portTransactionCost <- df$turnover * fees
  df$portRet <- df$portRet - df$portTransactionCost
}

# Form regression data for later (we will need this to calculate the portfolio Beta irrespective of transaction costs and obtain the Treynor Ratio)
RegressionData <- data.frame(matrix(ncol = 0, nrow = nrow(df)))    #Initialize a data frame with the appropriate number of rows and 0 columns
RegressionData$x <- benchmarks_truncated[,1]    #the market return without transaction costs is our independent variable, x
RegressionData$y <- df$portRet                  #portfolio return without transaction costs is our dependent variable, y

#COMPILE RESULTS##################################################################################################################################

# Let's start by calculating actual compounded annual portfolio return
calc_CAR <- function(ret){
  car <- prod(1+ret)-1 # Assuming x is simple return, add 1 for cumulative product calculation
  return(car)
}
df["CompoundAnnualReturn"] <- rollapply(df["portRet"], width=analysis_window, FUN = calc_CAR, fill=NA, align="right")

## At this point, we have two single-column vectors for portfolio returns, one monthly and one compounded annual.
## Now we can calculate portfolio analytics based on either single-column vector

#########################################################################################
#################################### M O N T H L Y ###################################### 
#########################################################################################
#The following analysis is executed over the single-column vector of MONTHLY portfolio-level returns


# Min/Max Monthly Return
df["Min_monthly"] <- rollapply(df["portRet"], width=analysis_window, FUN = min, fill=NA, align="right")
df["Max_monthly"] <- rollapply(df["portRet"], width=analysis_window, FUN = max, fill=NA, align="right")


# Monthly Standard Deviation
df["portStdDev_monthly"] <- rollapply(df["portRet"], width=analysis_window, FUN = sd, fill=NA, align="right")


# Monthly VaR
calc_VaR <- function(x, prob=CVaRpercentile, notional=1, min_obs=analysis_window) {
  non_na_data <- x[!is.na(x)]
  if (length(non_na_data) >= min_obs) {
    return(-quantile(non_na_data, prob) * notional)
  } else {
    return(NA)
  }
}
df["portVaR"] <- rollapply(df["portRet"], width=analysis_window, FUN = calc_VaR, fill=NA, align="right")


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
df["portCVaR"] <-rollapply(df["portRet"], width=analysis_window, FUN = calc_CVaR, fill=NA, align="right")


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


# Risk-Adjusted Annual Returns #

  
  # SHARPE RATIO (CFA GIPS standard for "ex post" SR)
  
    # Transfer monthly risk free rate to from benchmarks_truncated to df
    df["monthly_Rf"] <- benchmarks_truncated$t30ret 
    
    # Calculate the SR numerator
    meanPortRet <- rollapply(df["portRet"], width=analysis_window, FUN = mean, fill=NA, align="right") #Average Monthly Portfolio Return over prior 12 months
    meanRiskFreeRet <- rollapply(df["monthly_Rf"], width=analysis_window, FUN = mean, fill=NA, align="right") #Average Monthly Risk Free Rate of Return over prior 12 months
    SRnumerator <- meanPortRet - meanRiskFreeRet
    
    # Calculate the SR denominator as the standard deviation of portfolio returns over prior 12 months
    SRdemoninator <- rollapply(df["portRet"], width=analysis_window, FUN = sd, fill=NA, align="right")
    
    # Calculate the monthly Sharpe Ratio
    df["portSR"] <- SRnumerator / SRdemoninator
  
    
    
  
  # INFORMATION RATIO (CFA GIPS standard monthly IR)
  
    # Transfer monthly market rate to from benchmarks_truncated to df
    df["monthly_Rm"] <- benchmarks_truncated[,1]
    if (transaction_costs=='on') {df["monthly_Rm"] <- df$monthly_Rm - (benchmarks_truncated$spTurnover*fees)}
    
    # Calculate the IR numerator
    df["Rp_minus_Rm"] <- df$portRet - df$monthly_Rm # add excess "active" return to df
    IRnumerator <- rollapply(df["Rp_minus_Rm"], width=analysis_window, FUN = mean, fill=NA, align="right") #Average Monthly Excess "Active" Return over prior 12 months
    
    # Calculate the IR denominator as the standard deviation of those excess returns over prior 12 months
    IRdenominator <- rollapply(df["Rp_minus_Rm"], width=analysis_window, FUN = sd, fill=NA, align="right")
    
    # Calculate the monthly Information Ratio
    df["portIR"] <- IRnumerator / IRdenominator
    
    
  
  
  # TREYNOR RATIO
    
    # Calculate the TR numerator (=SR numerator)
    TRnumerator <- SRnumerator
    
    # Calculate the TR denominator as the regression coef. on S&P 500 monthly returns vs. portfolio monthly returns over prior 3 years
    
    df$portBeta <- NA  # Initializes the portBeta column with NAs
    n <- nrow(df)
    
    for (j in 36:n) {
      subset_RegressionData <- RegressionData[(j-36+1):j, ]   #Subset the last 36 months of regression data up to the current month
      SLR_model <- lm(y ~ x, data=subset_RegressionData)      #Fit the SLR model ignoring tranaction costs even if they are turned on (we shouldn't let transaction costs change our portfolio beta)
      df$portBeta[j] <- coef(SLR_model)[2]                    #Store the coefficient (beta) in portBeta
    }
    
    # Calculate the monthly Treynor Ratio
    df["portTR"] <- TRnumerator / df$portBeta
  
  
  
  
  
#########################################################################################
##################################### A N N U A L ####################################### 
#########################################################################################
#The following analysis is executed over the single-column vector of COMPOUNDED ANNUAL portfolio-level returns 
  
# Risk-Adjusted Annual Returns # 
  
  
  # SHARPE RATIO (Skyler's version)
  
    # Transfer monthly risk free rate to from benchmarks_truncated to df
    #df["monthly_Rf"] <- benchmarks_truncated$t30ret

    # Calculate the excess MONTHLY returns on the optimized portfolio over the risk-free rate
    #df["Rp_minus_Rf"] <- df$portRet - df$t30ret
    
    # Calculate the annualized standard deviation of excess monthly returns on the optimized portfolio = Sharpe Ratio denominator
    #SRdenominator <- rollapply(df$Rp_minus_Rf, width=analysis_window, FUN = sd, fill=NA, align="right")
    #SRdenominator <- SRdenominator*sqrt(12)
    
    # Calculate the actual excess annual portfolio return = Sharpe Ratio numerator
    #SRnumerator <- df$CompoundAnnualReturn - benchmarks_truncated$USGG10YR
    
    # Calculate Sharpe Ratio on a TTM rolling basis
    #df["portSR"] <- SRnumerator/SRdenominator
    
    
    
    
  # INFORMATION RATIO (Skyler's version)
    
    # Transfer monthly market rate to from benchmarks_truncated to df
    #df["monthly_Rm"] <- benchmarks_truncated[,1]
    #if (transaction_costs=='on') {df["monthly_Rm"] <- df$monthly_Rm - (benchmarks_truncated$spTurnover*fees)}
    
    # Calculate the excess MONTHLY returns on the optimized portfolio over the S&P 500 index
    #df["Rp_minus_Rm"] <- df$portRet - df$monthly_Rm
    
    # Calculate the annualized standard deviation of excess monthly returns on the optimized portfolio = Information Ratio denominator
    #IRdenominator <- rollapply(df$Rp_minus_Rm, width=analysis_window, FUN = sd, fill=NA, align="right")
    #IRdenominator <- IRdenominator*sqrt(12)
    
    # Calculate the actual excess annual portfolio return = Information Ratio numerator
    #CompoundAnnualReturn_MKT <- rollapply(df$monthly_Rm, width=analysis_window, FUN = calc_CAR, fill=NA, align="right")
    #IRnumerator <- df$CompoundAnnualReturn - df$CompoundAnnualReturn_MKT
    
    # Calculate Sharpe Ratio on a TTM rolling basis
    #df["portIR"] <- IRnumerator/IRdenominator
    
    
    
    
  # TREYNOR RATIO (Skyler's version)
    
    # Calculate annual monthly beta of the optimized portfolio vs. S&P 500 index over 12 months = Treynor Ratio denominator
   
    #df$Rm_minus_Rf <- df$monthly_Rm - df$monthly_Rf
    #df$portBeta <- NA  # Initializes the portBeta column with NAs
    #n <- nrow(df)
    
    #for (j in (analysis_window):n) {
    #  subset_RegressionData <- RegressionData[(j-analysis_window+1):j, ]    #Subset the last 36 months of regression data up to the current month
    #  SLR_model <- lm(y ~ x, data=subset_RegressionData)                    #Fit the SLR model ignoring tranaction costs even if they are turned on (we shouldn't let transaction costs change our portfolio beta)
    #  df$portBeta[j] <- coef(SLR_model)[2]                                  #Store the coefficient (beta) in portBeta
    #}


    # Calculate the optimized portfolio's Treynor Ratio
    #df["portTR"] = SRnumerator/df$portBeta

    
################################################################################################################ 
################################################################################################################ 
# Annualize risk-adjusted return measures if annualizeRiskAdjRet=='yes'
    
   if (annualizeRiskAdjRet=='yes') {
     df["portSR"] <- df["portSR"]*sqrt(12)
     df["portIR"] <- df["portIR"]*sqrt(12)
     df["portTR"] <- df["portIR"]*12
       
   }
    
#WRITE_THE_RESULTS_TO_WORKING_DIRECTORY###########################################################################################################
write.csv(df, paste0(filename, ".csv"))
write.csv(df_weights_bgn, paste0(filename, "_weights.csv"))
