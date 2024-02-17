# YieldBasedAssetRatio

A yield-based asset ratio to boost minimum investment returns.

This software generates results consumed by the `YieldBasedAssetRatio.Rmd` script to produce `YieldBasedAssetRatio.pdf`.

## How to generate inputs for YieldBasedAssetRatio.Rmd

```
# in R
source("fetch_shiller.R")

# in bash
sqlite3 < graham_parm.sql

# in R
source("render_figures.R")
```

### To modify parameters

Edit the `e10p_margin` case on line 41 of `render_figures.R`.

# Disclaimer

Arthur Eschenlauer is not a financial advisor. This material is not investment advice, a solicitation, or a recommendation to buy or sell any security or investment product; it has been provided only for purposes of academic investigation.
