WARNING!
A significant shortcoming of my thesis submission is the realistic modeling of portfolio turnover and transaction costs. Referencing lines 363-366 of PortfolioBacktester.R you can see that portfolio turnover is approximated rather than exact. That's because df_weights contains the "optimal" weighting scheme as a result of monthly rebalancing, as opposed to the actual MoM change in a portfolio’s composition due to fluctuations in asset prices. For example, if you ran an equally weighted portfolio though PortfolioBacktester.R transaction costs would equal zero (which is obviously incorrect). Future research will address this issue.

UPDATE!
