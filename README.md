# YieldBasedAssetRatio

A yield-based asset ratio to boost minimum investment returns.

This software generates results consumed by the `YieldBasedAssetRatio.Rmd` script to produce `YieldBasedAssetRatio.pdf`, for which the abstract reads:

> Minimum investment returns are of primary importance to funding retirement expenses from an investment portfolio. Prolonged periods of low yields have at times had severe effects on minimum 20-year returns. Is there a strategy that can mitigate weak medium-term returns more effectively than fixed-percentage stock allocations?
>
> Considering Benjamin Graham's "Margin of Safety" principle for stock purchases and its complement for stock sales, one might adjust the stock percentage of the portfolio based on both the stock earnings yield and the current yield for bonds, facilitating capital appreciation by avoiding trading until the present yield of the purchased security is substantially greater than that of the sold security; it may be wise to add a hedge against stock prices in excess of historically sustainable levels.
>
> For 20-year intervals since 1911, when allocating between 10-year US Treasury bonds and the S&P 500 index, a simple formula implementing such a "Yield-Based Asset Ratio" hypothetically would have had a *minimum* compound annual growth rate (CAGR) of 1.92%, considerably higher than would have been observed for 6%, 60%, and 85% stock allocations (CAGR -2.33%, -0.41%, and 0.09%, respectively). Results suggest that a fixed-percentage stock allocation may not offer the best protection of retirement income for interval lengths of 11 or more years.

## How to generate inputs for YieldBasedAssetRatio.Rmd

```
# in R
source("fetch_shiller.R")

# in bash
sqlite3 graham.sqlite < graham_parm.sql

# in R
source("render_figures.R")

# in RStudio, knit YieldBasedAssetRatio.Rmd
```

### To modify parameters

Edit the `e10p_margin` case on line 45 of `render_figures.R`.

# Disclaimer

Arthur Eschenlauer is not a financial advisor. This material is not investment advice, a solicitation, or a recommendation to buy or sell any security or investment product; it has been provided only for purposes of academic investigation.
