[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18387394.svg)](https://zenodo.org/doi/10.5281/zenodo.10764741)

# YieldBasedAssetRatio

### Software for two Yield-Based Asset Ratio (YBAR) papers

This software generates:

- results consumed by the `YieldBasedAssetStockBondCash.Rmd` script to produce `YieldBasedAssetStockBondCash.pdf` (Available at SSRN: [https://ssrn.com/abstract=4746302](https://ssrn.com/abstract=4746302) or [https://dx.doi.org/10.2139/ssrn.6074226](https://dx.doi.org/10.2139/ssrn.6074226)).

- results consumed by the `YieldBasedAssetRatio.Rmd` script to produce `YieldBasedAssetRatio.pdf` (Available at SSRN: [https://ssrn.com/abstract=4746302](https://ssrn.com/abstract=4746302) or [https://dx.doi.org/10.2139/ssrn.4746302](https://dx.doi.org/10.2139/ssrn.4746302)).

## How to generate `YieldBasedAssetRatio.pdf` and `YieldBasedStockBondCash.pdf`

To build from the command line (rather than RStudio), proceed as follows:

## Set up the conda enviroment to support `rmarkdown`

To bootstrap conda, it may be convenient to install and use "MiniForge" as described at

> [https://github.com/conda-forge/miniforge](https://github.com/conda-forge/miniforge).

### Build an environment that can generate the PDF

```bash
. ~/conda/bin/activate
mamba create -n graham -c conda-forge \
  ca-certificates \
  make \
  openssl \
  r-base=4.3.3 \
  r-bookdown \
  r-latex2exp \
  r-readods \
  r-readxl \
  r-renv=1.1.5 \
  r-sqldf \
  r-svglite \
  r-tinytex \
  r-kableextra
```

### Activate the environment that can generate the PDF

```bash
. ~/conda/bin/activate graham
```

### Ensure that `sqlite3` is on your PATH

This build depends upon the `sqlite3` command line utility, which is not available (as far as I know) from the conda-forge conda channel.  This utility is documented at [https://sqlite.org/cli.html](https://sqlite.org/cli.html) and may be obtained by downloading the `sqlite-tools-...` bundles from [https://sqlite.org/download.html](https://sqlite.org/download.html) if `sqlite3` is not already installed on your system.

Make sure that `sqlite3` is on your path when performing the following steps.

### Steps directed by `Makefile`

At this point, 

```bash
make -f Makefile
```
should build `YieldBasedAssetRatio.pdf` and `YieldBasedStockBondCash.pdf`

Conceptually, the `Makefile` directs `make` to perform the following:

```bash
## build YieldBasedAssetRatio.pdf
    # fetch Shiller dataset and extract initial graham.sqlite database
    R --vanilla --no-echo -f fetch_shiller.R
    # run DDL to create needed views
    sqlite3 graham.sqlite < graham_parm.sql
    # create the figures for the PDF
    R --vanilla --no-echo -f render_figures.R
    # create YieldBasedAssetRatio.pdf
    R --vanilla --no-echo -f build.R

## build YieldBasedAssetStockBondCash.pdf
    # fetch Shiller dataset thru 2023
    R --vanilla --no-echo -f fetch_shiller_sbc.R
    # run R to set up for DDL
    R --vanilla --no-echo -f seed_graham_gs10_sbc.R
    # run DDL to create needed views
    sqlite3 graham_sbc.sqlite < graham_parm_gs10_sbc.sql
    # create the figures for the PDF
    R --vanilla --no-echo -f render_figures_gs10_sbc.R
    # create YieldBasedAssetStockBondCash.pdf
    R --vanilla --no-echo -f build_sbc.R
```

The `Makefile` directs similar steps to build `YieldBasedStockBondCash.pdf`

### To modify parameters

1. Edit:
    - lines 46-140 of `render_figures.R` to change parameters for YieldBasedAssetRatio.pdf
    - lines 87-109 and lines 167-183  of `render_figures_sbc.R` to change parameters for `YieldBasedStockBondCash.pdf`
2. Run `make -f Makefile`

<!--
For documentation of the parameters, see endnotes 7 and 8 of [https://eschenlauer.com/investing/risk_based_allocation/YBAR_intro.html](https://eschenlauer.com/investing/risk_based_allocation/YBAR_intro.html) (and elsewhere in that document for the theoretical bases for these parameters).

## How to generate figures for `YBAR_intro.html`

To generate the figures referenced by `YBAR_intro.html`:

```bash
make -f Makefile ybar_intro
```
-->

# Disclaimer

Arthur Eschenlauer is not a financial advisor. This material is not investment advice, a solicitation, or a recommendation to buy or sell any security or investment product; it has been provided only for purposes of academic investigation, and it has not been reviewed or endorsed by financial professionals.
