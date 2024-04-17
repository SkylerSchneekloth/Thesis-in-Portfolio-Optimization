rm(list = ls())
setwd("Path/To/Your/Working/Directory")

# Set analysis window
analysisWindow <- 'all'    # correlation matrix will reference the [analysisWindow] most recent months of data
# use 'all' to exclude none

# Read in cluster data
clusterData <- read.csv("clusterData_RE_100sims_2pt5max.csv", header = TRUE)

# Read in libraries
library(quantmod)
library(timeSeries)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(fPortfolio)
library(caTools)
library(xts)
library(dplyr)
library(zoo)
library(tidyverse) # required for the Graphical Network Analysis
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
library(igraph) # required for the Graphical Network Analysis
library(ggraph) # required for the Graphical Network Analysis

# Read in crsp returns data

returnsType <- 2

if (returnsType == 1) {
  
  #price returns
  returns <- read.csv("crsp_PriceReturnsFile.csv", header=TRUE, row.names=1)
  if (analysisWindow != 'all') {returns <- tail(returns, analysisWindow)}
  returns <- returns[, colnames(returns) %in% clusterData$Ticker]
  print("you successfully imported asset-level PRICE returns from the crsp database")
  
} else if (returnsType == 2) {
  
  #total return
  returns <- read.csv("crsp_TotalReturnsFile.csv", header=TRUE, row.names=1)
  if (analysisWindow != 'all') {returns <- tail(returns, analysisWindow)}
  returns <- returns[, colnames(returns) %in% clusterData$Ticker]
  print("you successfully imported asset-level TOTAL returns from the crsp database")
  
} else {
  print("ERROR: change returnsType=1 for price return, or returnsType=2 for total return")
}

# Calculate correlations for the Graphical Network
assetCorrelations <- cor(returns, use = "pairwise.complete.obs") # ignores NAs on a pairwise basis




###############################################################################
# Visualize factor clusters from Graphical Network Analysis
###############################################################################


# Convert the correlation matrix to a data frame
edge_list <- as.data.frame(as.table(assetCorrelations))

# Rename columns for clarity
colnames(edge_list) <- c("from", "to", "correlation")

# Filter out self-loops and, optionally, very low correlations
edge_list <- edge_list[edge_list$from != edge_list$to & abs(edge_list$correlation) > 0.2,]

# Create a nodes data frame
node_data <- data.frame(
  name = rownames(assetCorrelations),
  weight = clusterData$Weight[match(rownames(assetCorrelations), clusterData$Ticker)]
)

# Set names as factor to ensure matching
edge_list$from <- factor(edge_list$from, levels = node_data$name)
edge_list$to <- factor(edge_list$to, levels = node_data$name)


# Create an igraph object
graph <- graph_from_data_frame(d = edge_list, vertices = node_data, directed = FALSE)


# Plotting the Asset Correlation Network
ggraph(graph, layout = 'fr') +
  geom_edge_link(aes(width = abs(correlation), edge_alpha = abs(correlation)), color = "grey") +
  geom_node_point(aes(size = weight), color = "darkred", show.legend = TRUE) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph()
