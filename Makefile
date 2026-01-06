# Crudely attempted Makefile
#   Note well:
#   - requires sqlite3 on your path
#   - requires an activated conda environment, e.g.:
#     . ~/conda/bin/activate
#     mamba create -n graham -c conda-forge r-base=4.3.3 r-renv=1.1.5 \
#                                           r-tinytex r-bookdown r-sqldf \
#                                           r-latex2exp r-readxl \
#                                           r-readods \
#                                           make ca-certificates openssl

all: YieldBasedStockBondCash.pdf YieldBasedAssetRatio.pdf

YieldBasedStockBondCash.pdf: \
    graham_sbc.sqlite \
    YieldBasedStockBondCash.Rmd \
    render_figures_sbc.R \
    render_figures_gs10_sbc.R \
    build_sbc.R
	R --vanilla --no-echo -f render_figures_gs10_sbc.R
	R --vanilla --no-echo -f build_sbc.R

YieldBasedAssetRatio.pdf: \
    graham.sqlite \
    YieldBasedAssetRatio.Rmd \
    render_figures.R \
    build.R
	R --vanilla --no-echo -f render_figures.R
	R --vanilla --no-echo -f build.R

ybar_intro: \
    graham.sqlite \
    old_paper_figures.R \
    render_figures.R
	R --vanilla --no-echo -f old_paper_figures.R

graham_sbc.sqlite: seed_graham_gs10_sbc.R fetch_shiller_sbc.R graham_parm_gs10_sbc.sql
	touch graham_sbc.sqlite
	if [ -e graham_sbc.sqlite ]; then rm graham_sbc.sqlite ; fi
	if [ -e ie_data.xls ]; then rm ie_data.xls ; fi
	R --vanilla --no-echo -f fetch_shiller_sbc.R
	R --vanilla --no-echo -f seed_graham_gs10_sbc.R
	sqlite3 graham_sbc.sqlite < graham_parm_gs10_sbc.sql

graham.sqlite: graham_parm.sql fetch_shiller_sbc.R
	touch graham.sqlite
	if [ -e graham.sqlite ]; then rm graham.sqlite ; fi
	if [ -e ie_data.xls ]; then rm ie_data.xls ; fi
	R --vanilla --no-echo -f fetch_shiller.R
	sqlite3 graham.sqlite < graham_parm.sql

# vim: set noet sw=2 ts=2 :
