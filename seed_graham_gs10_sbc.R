# GS10
BOND_DUR <- 10.0
# model GS10 yield as 0.0 * SP500 + 1.0 * GS10
COEF_SP500 <- 0.0
COEF_GS10 <-  1.0
BOND_NAME <- "GS10"
local_db <- "graham_sbc.sqlite"
RECENT_CUTOFF <- 2025

source("seed_graham_sbc.R")