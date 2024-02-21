# Crudely attempted Makefile
#   Note well:
#   - requires sqlite3 on your path
#   - requires an activated conda environment, e.g.:
#     . ~/conda/bin/activate
#     mamba create -n graham -c conda-forge r-renv=1.0.3 \
#                                           r-tinytex r-bookdown r-sqldf \
#                                           r-latex2exp r-readxl \
#                                           make ca-certificates openssl

YieldBasedAssetRatio.pdf: \
    graham.sqlite \
    YieldBasedAssetRatio.Rmd \
    render_figures.R \
    build.R
	R --vanilla --no-echo -f render_figures.R
	R --vanilla --no-echo -f build.R

graham.sqlite: graham_parm.sql fetch_shiller.R ie_data.xls
	R --vanilla --no-echo -f fetch_shiller.R
	sqlite3 graham.sqlite < graham_parm.sql

ie_data.xls: fetch_shiller.R
	R --vanilla --no-echo -f fetch_shiller.R

# vim: set noet sw=2 ts=2 :
