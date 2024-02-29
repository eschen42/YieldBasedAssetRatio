my_variants <-
  c(
    "e10p",         # 1
    "ep",           # 2
    "ep_margin",    # 3
    "e10p_margin",  # 4
    "ybar_intro"     # 5
    )

# note: "ybar_intro" generates images for
#  https://eschenlauer.com/investing/risk_based_allocation/YBAR_intro.html
my_variant_selector <- c(1:5)

source("render_figures.R")