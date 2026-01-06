VCLT_PRM <- 0
img_dir <- "img_sbc"
if (!dir.exists(img_dir)) {
  dir.create(img_dir)
}
GRAHAM_DBNAME <- "graham_sbc.sqlite"
BOND_NAME <- "GS10"

RECENT_CUTOFF <- 2025
DETAIL_CUTOFF_SELECTOR <- c(1,2,3)[2]
#DETAIL_CUTOFF_DATE <- c(1993.75,2017.00,2002.75)[DETAIL_CUTOFF_SELECTOR]
DETAIL_CUTOFF_DATE <- c(1993.75,2019.75,2002.75)[DETAIL_CUTOFF_SELECTOR]
DETAIL_CUTOFF_NORM <- c(1363,1666,1463)[DETAIL_CUTOFF_SELECTOR]
DETAIL_LEGEND_TOP <- c("topleft","topleft","left")[DETAIL_CUTOFF_SELECTOR]
DETAIL_LEGEND_MIDDLE <- c("left","top","top")[DETAIL_CUTOFF_SELECTOR]
DETAIL_LEGEND_BOTTOM <- c("topleft","topright","topleft")[DETAIL_CUTOFF_SELECTOR]

my_variant_selector <- c(3, 4)[1]
source("render_figures_sbc.R")

my_variant_selector <- c(3, 4)[2]
source("render_figures_sbc.R")

rm(my_variant_selector)