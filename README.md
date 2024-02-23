# YieldBasedAssetRatio

A yield-based asset ratio to boost minimum investment returns.

This software generates results consumed by the `YieldBasedAssetRatio.Rmd` script to produce `YieldBasedAssetRatio.pdf`, for which the abstract reads:

> Minimum investment returns are of primary importance to funding retirement expenses from an investment portfolio. Prolonged periods of low yields have at times had severe effects on minimum 20-year returns. Is there a strategy that can mitigate weak medium-term returns more effectively than fixed-percentage stock allocations?
>
> Benjamin Graham allowed for fluctuation in the proportion of a portfolio invested in common stock.
> Considering his "Margin of Safety" principle for stock purchases and its complement for stock sales, one might adjust the stock percentage of the portfolio based on both the stock earnings yield and the current yield for bonds, facilitating capital appreciation by avoiding trading until the present yield of the purchased security is substantially greater than that of the sold security.
> It may be wise to add a hedge against stock prices in excess of historically sustainable levels.
>
> For 20-year intervals since 1911, when allocating between 10-year US Treasury bonds and the S&P 500 index, a simple formula implementing such a "Yield-Based Asset Ratio" hypothetically would have had a *minimum* real compound annual growth rate (CAGR) of 1.92%, considerably higher than would have been observed for 6%, 60%, and 85% stock allocations (CAGR -2.33%, -0.41%, and 0.09%, respectively). Results suggest that a fixed-percentage stock allocation may not offer the best protection of investment returns and principal for interval lengths of 11 or more years.

## How to generate YieldBasedAssetRatio.Rmd

To build from the command line (rather than RStudio), first set up the conda enviroment to support `rmarkdown`.  (To bootstrap conda, I install and use "MiniForge" as described at [https://github.com/conda-forge/miniforge](https://github.com/conda-forge/miniforge).)

### Build an environment that can generate the PDF
```bash
. ~/conda/bin/activate
mamba create -n graham -c conda-forge r-renv=1.0.3 r-tinytex r-bookdown r-sqldf \
                                      r-latex2exp r-readxl \
                                      make ca-certificates openssl
```

### Activate the environment that can generate the PDF
```bash
. ~/conda/bin/activate graham
```

### Ensure that `sq1ite3` is on your PATH

This build depends upon the `sqlite3` command line utility, which is not available (as far as I know) from the conda-forge conda channel.  This utility is documented at [https://sqlite.org/cli.html](https://sqlite.org/cli.html) and may be obtained by downloading the `sqlite-tools-...` bundles from [https://sqlite.org/download.html](https://sqlite.org/download.html) if `sqlite3` is not already installed on your system.  Make sure that it is on your path when performing the following steps.

### Steps directed by `Makefile`

At this point, 
```bash
make -f Makefile
```
should build `YieldBasedAssetRatio.pdf`

Conceptually, the `Makefile` directs `make` to perform the following:

```bash
# fetch Shiller dataset and extract initial graham.sqlite database
R --vanilla --no-echo -f fetch_shiller.R
# run DDL to create needed views
sqlite3 graham.sqlite < graham_parm.sql
# create the figures for the PDF
R --vanilla --no-echo -f render_figures.R
# create the PDF
R --vanilla --no-echo -f build.R
```

### To modify parameters

Edit the `e10p_margin` case on line 45 of `render_figures.R` and `make`.

# Disclaimer

Arthur Eschenlauer is not a financial advisor. This material is not investment advice, a solicitation, or a recommendation to buy or sell any security or investment product; it has been provided only for purposes of academic investigation.
