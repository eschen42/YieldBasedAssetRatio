#img_dir <- "img"
X_MAX <- 2025
if (!exists("my_variants")) {
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
}
if (!exists("my_variant_selector")) {
  my_variant_selector <- c(3, 4)[2]
}

if (!dir.exists(img_dir)) { dir.create(img_dir)}
cp_img <-
  function(src, dst) {
    if(file.exists(src)) {
      file.copy(from = src, to = paste0(img_dir, "/", dst), overwrite = TRUE)
      }
    }

# images for
#  https://eschenlauer.com/investing/risk_based_allocation/YBAR_intro.html
if (!dir.exists("img_ybar_intro")) { dir.create("img_ybar_intro")}
cp_old <-
  function(src, dst) {
    #cat("cp_old ", src, dst, "\n", file = stderr())
    if(file.exists(src)) {
      file.copy(from = src, to = paste0("img_ybar_intro/", dst), overwrite = TRUE)
      }
    }

grid_four <-
  function(
    # side must be one of 1:4 bottom left top right
    side,
    col = "lightgray",
    lty = c(1,3,2,3), 
    lwd = par("lwd") * c(0.75, 0.75, 1, 0.75)
  ) {
    my_at <-
      sort(
        c(
          axTicks(side),
          axTicks(side) + 0.0025,
          axTicks(side) + 0.005,
          axTicks(side) + 0.0075,
          axTicks(side) + 0.01,
          axTicks(side) + 0.0125,
          axTicks(side) + 0.015,
          axTicks(side) + 0.0175
          )
        )
    if (side %% 2 > 0) {
      # 1 (below) or 3 (above)
      invisible(abline(v = my_at, col = col, lty = lty, lwd = lwd))
    } else {
      # 2 (left) or 4 (right)
      invisible(abline(h = my_at, col = col, lty = lty, lwd = lwd))
    }
  }


for_variants <- my_variants[my_variant_selector]
for (my_variant in for_variants) {
  if ((my_variant %in% c("ep", "ybar_intro")) || length(my_variant_selector) > 1) {
    img_ext <- function(s) paste0(s, ".svg")
    img <- svg
    img_type_name <- "svg"
  } else {
    img_ext <- function(s) paste0(s, ".pdf")
    img <- pdf
    img_type_name <- "pdf"
  }
  # see notes 7 and 8 of YBAR_intro.html for an explanation of the parameters set here.
  zero_cut <- sprintf("%0.4f", 0.0275)
  update_sql <-
    switch (
      my_variant,
      e10p_margin = 
        paste0("
        UPDATE parms
          SET H    = 0.25,   -- maximum tolerable 'loss on paper' when past margin of reversion
              Ma   = 0.85,   -- maximum acceptable stock proportion in portfolio
              Mi   = 0.06,   -- minimum acceptable stock proportion in portfolio
              mid  = 0.60,   -- mid-way allocation (avg YBAR for 1911-2022 with [Mi,Ma] = [0.06,0.88])
              -- R    = 0.012,  -- offset minimum stock pct; lowering decreases capital appreciation
              T    = 0.0414, -- historic bond CAGR
              -- W    = 0.0645, -- historic stock CAGR
              -- Y    = 0.0400, -- the mean of W and the historic T-bill CAGR (1.54%)
              -- Z    = 0.0303, -- 1 / (95th percentile for P/E10)
              trgr = 0.06,   -- amount beyond limit when is trade triggered
              first_year = 1911,   -- first year for rolling period calculations
              -- scheme_S = 'ep', -- 'ep' use E/P for stock earnings yield (S)
              scheme_S = 'e10p', -- 'e10p' use E10/P for stock earnings yield (S)
              scheme_M = 'margin', -- 'margin' use margins of safety and folly for threshold-setting
              min_factor = 2, -- slope-accelerator for min stock percentage (2 works well since 1911)
              -- min_factor = 1.8, -- slope-accelerator for min stock percentage (2 works well since 1911)
              max_factor = 3, -- slope-accelerator for max stock percentage (3 works well since 1911)
              lt_lim_cutoff = ", zero_cut, ", -- long-term limit cutoff based on GS10
              lt_lim_slope = 3.1,     -- long-term limit slope based on GS10
              stock_lo = 0.3,
              stock_hi = 0.6
          WHERE grp = 1
        "),
      ybar_intro = 
        ("
        UPDATE parms
          SET H    = 0.25,   -- maximum tolerable 'loss on paper' when past margin of reversion
              Ma   = 0.85,   -- maximum acceptable stock proportion in portfolio
              Mi   = 0.06,   -- minimum acceptable stock proportion in portfolio
              mid  = 0.60,   -- mid-way allocation (avg YBAR for 1911-2022 with [Mi,Ma] = [0.06,0.88])
              R    = 0.012,  -- offset minimum stock pct; lowering decreases capital appreciation
              T    = 0.0414, -- historic bond CAGR
              W    = 0.0645, -- historic stock CAGR
              Y    = 0.0400, -- the mean of W and the historic T-bill CAGR (1.54%)
              Z    = 0.0303, -- 1 / (95th percentile for P/E10)
              trgr = 0.06,   -- amount beyond limit when is trade triggered
              first_year = 1911,   -- first year for rolling period calculations
              scheme_S = 'e10p', -- 'e10p' use E10/P for stock earnings yield (S)
              scheme_M = 'RTWYZ' -- 'RTWYZ' use R, T, W, Y, Z for threshold-setting
              WHERE grp = 1
        "),
      e10p = 
        ("
        UPDATE parms
          SET H    = 0.25,   -- maximum tolerable 'loss on paper' when past margin of reversion
              Ma   = 0.85,   -- maximum acceptable stock proportion in portfolio
              Mi   = 0.06,   -- minimum acceptable stock proportion in portfolio
              mid  = 0.60,   -- mid-way allocation (avg YBAR for 1911-2022 with [Mi,Ma] = [0.06,0.88])
              R    = 0.012,  -- offset minimum stock pct; lowering decreases capital appreciation
              T    = 0.0414, -- historic bond CAGR
              W    = 0.0645, -- historic stock CAGR
              Y    = 0.0400, -- the mean of W and the historic T-bill CAGR (1.54%)
              Z    = 0.0303, -- 1 / (95th percentile for P/E10)
              trgr = 0.06,   -- amount beyond limit when is trade triggered
              first_year = 1911,   -- first year for rolling period calculations
              scheme_S = 'e10p', -- 'e10p' use E10/P for stock earnings yield (S)
              scheme_M = 'RTWYZ' -- 'RTWYZ' use R, T, W, Y, Z for threshold-setting
              WHERE grp = 1
        "),
      ep = 
        ("
        UPDATE parms
          SET H    = 0.25,   -- maximum tolerable 'loss on paper' when past margin of reversion
              Ma   = 0.85,   -- maximum acceptable stock proportion in portfolio
              Mi   = 0.06,   -- minimum acceptable stock proportion in portfolio
              mid  = 0.60,   -- mid-way allocation (avg YBAR for 1911-2022 with [Mi,Ma] = [0.06,0.88])
              R    = 0.012,  -- offset minimum stock pct; lowering decreases capital appreciation
              T    = 0.0414, -- historic bond CAGR
              W    = 0.0645, -- historic stock CAGR
              Y    = 0.0400, -- the mean of W and the historic T-bill CAGR (1.54%)
              Z    = 0.0303, -- 1 / (95th percentile for P/E10)
              trgr = 0.06,   -- amount beyond limit when is trade triggered
              first_year = 1911,   -- first year for rolling period calculations
              scheme_S = 'ep', -- 'ep' use E/P for stock earnings yield (S)
              scheme_M = 'RTWYZ' -- 'RTWYZ' use R, T, W, Y, Z for threshold-setting
              WHERE grp = 1
        "),
      ep_margin = 
        paste0("
        UPDATE parms
          SET H    = 0.25,   -- maximum tolerable 'loss on paper' when past margin of reversion
              Ma   = 0.85,   -- maximum acceptable stock proportion in portfolio
              Mi   = 0.06,   -- minimum acceptable stock proportion in portfolio
              mid  = 0.60,   -- mid-way allocation (avg YBAR for 1911-2022 with [Mi,Ma] = [0.06,0.88])
              T    = 0.0414, -- historic bond CAGR
              trgr = 0.06,   -- amount beyond limit when is trade triggered
              first_year = 1911,   -- first year for rolling period calculations
              scheme_S = 'ep', -- 'ep' use E/P for stock earnings yield (S)
              scheme_M = 'margin', -- 'margin' use margins of safety and folly for threshold-setting
              min_factor = 2, -- slope-accelerator for min stock
              max_factor = 3, -- slope-accelerator for max stock
              lt_lim_cutoff = ", zero_cut, ", -- long-term limit cutoff based on GS10
              lt_lim_slope = 3.1,     -- long-term limit slope based on GS10
              stock_lo = 0.3,
              stock_hi = 0.6
              WHERE grp = 1
        ")
    )
  
  zerocut_sql <-
    sub(
      sprintf("lt_lim_cutoff = %s", zero_cut),
      "lt_lim_cutoff = 0",
      update_sql
      )

  my_dbname <- GRAHAM_DBNAME
  my_setparm_f <-
    function(., sql = update_sql) {
    my_conn <- RSQLite::dbConnect(RSQLite::SQLite(), my_dbname)
    on.exit(RSQLite::dbDisconnect(my_conn))
    RSQLite::dbExecute(my_conn, sql)
  }
  my_dbname |> my_setparm_f(sql = zerocut_sql)
  zerogrow_df <-
    sqldf::sqldf(
      "select * from grow",
      dbname = my_dbname
    )
  zerogrowth_df <-
    sqldf::sqldf(
      "select * from growth",
      dbname = my_dbname
    )
  zerogrowth_min_df <-
    sqldf::sqldf(
      "select * from growth_min",
      dbname = my_dbname
      )
  my_dbname |> my_setparm_f()

  if (img_type_name == "svg") {
    my_sub <- switch(
      my_variant,
      ybar_intro = "stock earnings yield = E10 / P",
      e10p = "stock earnings yield = E10 / P",
      ep = "stock earnings yield = E / P",
      e10p_margin = "margin-based; stock earnings yield = E10 / P",
      ep_margin = "margin-based; stock earnings yield = E / P"
      )
  } else {
    my_sub <- switch(
      my_variant,
      ybar_intro = "stock earnings yield = E10 / P",
      e10p = "stock earnings yield = E10 / P",
      ep = "stock earnings yield = E / P",
      e10p_margin = "stock earnings yield = E10 / P",
      ep_margin = "stock earnings yield = E / P"
      )
  }

  graham_base_df <-
    sqldf::sqldf(
      "select * from graham_base",
      dbname = my_dbname
      )
  
  if (FALSE) {
    var_delta <-
      function(.) {
        foo <- (.)
        ix1 <- 1:(length(foo) - 1)
        ix2 <- 2:(length(foo))
        var((foo[ix2] - foo[ix1]) / foo[ix1])
      }
    var_E <-
      var_delta(graham_base_df$Earnings_E[361:nrow(graham_base_df)])
    var_E10 <-
      var_delta(
        graham_base_df$S_P_Comp__P[361:nrow(graham_base_df)] /
          graham_base_df$pe10[361:nrow(graham_base_df)]
      )
    var_P <-
      var_delta(graham_base_df$S_P_Comp__P[361:nrow(graham_base_df)])
  }
  
  expected_pe_df <-
    sqldf::sqldf(
      "select * from expected_pe",
      dbname = my_dbname
      )
  expected_pe_intercept <-
    expected_pe_df[expected_pe_df$trend == "P/E10", "intercept"]
  expected_pe_slope <-
    expected_pe_df[expected_pe_df$trend == "P/E10", "annual_growth"]
  
  svg_path <- img_ext("fig10_PE10_trend")
  with(
    graham_base_df,
    {
      img(svg_path, width = 800/72, height = 400/72)
      plot(
        x = Date_Fraction,
        y = pe10,
        ylab = "P/E10 or CAPE",
        main = "Expected P/E10 has gradually increased",
        xlab = paste(
          "expected P/E10 = ",
          signif(expected_pe_intercept, 8),
          "+ (year)(",
          signif(expected_pe_slope, 8),
          ")"),
        type = "l"
      )
      lines(
        x = Date_Fraction[!is.na(pe10)],
        y = expected_pe_intercept +
            expected_pe_slope * Date_Fraction[!is.na(pe10)],
        col = "red",
        lty = "dashed"
      )
      legend(
        x = "topleft",
        legend = c(
          "P/E10",
          "Expected P/E10 (\"Tukey line fit\")"
          ),
        cex = 0.85,
        lty = c("solid", "dashed"),
        col = c("black", "red")
        )
      dev.off()
    }
  )
  
  relative_growth_min2max_df <-
    sqldf::sqldf(
      "select * from relative_growth_min2max",
      dbname = my_dbname
      )

  relative_growth_min_df <-
    sqldf::sqldf(
      "select * from relative_growth_min",
      dbname = my_dbname
      )

  growth_min2max_df <-
    sqldf::sqldf(
      "select * from growth_min2max",
      dbname = my_dbname
      )

  growth_min_df <-
    sqldf::sqldf(
      "select * from growth_min",
      dbname = my_dbname
      )
  growth_min_df$min_grow_mid <- zerogrowth_min_df$min_grow_ybar

  growth_df <-
    sqldf::sqldf(
      "select * from growth",
      dbname = my_dbname
    )
  growth_df$grow_mid <- zerogrowth_df$grow_ybar

  grow_df <-
    sqldf::sqldf(
      "select * from grow",
      dbname = my_dbname
    )
  grow_df$total_mid <- zerogrow_df$total_ybar
  
  if (TRUE) {
    if (file.exists("grow_df.ods")) {
      file.remove("grow_df.ods")
    }
    readODS::write_ods(grow_df, "grow_df.ods")
  }

  grow_1911 <- grow_df[
    grow_df$DateFraction > 1911 &
    grow_df$DateFraction < 2025
    ,
    ]

  if (TRUE) {
    if (file.exists("grow_1911.ods")) {
      file.remove("grow_1911.ods")
    }
    readODS::write_ods(grow_1911, "grow_1911.ods")
  }

  avg_alloc_df <-
    sqldf::sqldf(
      "SELECT avg(alloc) AS 'Average allocation since 1911'
        FROM grow
        WHERE DateFraction > 1911;",
      dbname = my_dbname
    )

  avg_alloc_1961_df <-
    sqldf::sqldf(
      "SELECT avg(alloc) AS 'Average allocation since 1961'
        FROM grow
        WHERE DateFraction > 1961;",
      dbname = my_dbname
    )

  parm_df <-
    sqldf::sqldf(
      "select * from parm",
      dbname = my_dbname
    )
  BOND_NAME <- parm_df$bond_name

  parm_coda <-
    sprintf("T = %0.2f%s; H = %0.2f", 100 * parm_df$T, "%", parm_df$H)
  parm_coda_H <- sprintf("H = %0.2f", parm_df$H)

  my_scramble <- c(1, 3, 2, 4:7)
  no_bills <- c(1:5, 7:8)
  no_bills477 <- c(1:4, 7:8)

  # Line types can either be specified as an integer
  #  (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash)
  #            "solid"            "44"      "13"      "1314"     "73"        "2262"
  # or as one of the character strings
  #  "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash",
  #     where "blank" uses ‘invisible lines’ (i.e., does not draw them).
  #my_lty <- c("solid",      "dotted",         "solid",       "dashed",     "solid",  "dotdash",  "longdash")
  #my_lty <- c("solid",      "121242",         "solid",       "dotdash",     "solid",  "dotted",  "72")
  my_lty <- c("solid",      "121242",         "solid",       "dotdash",     "solid",  "dotted",  "83")
  my_col <- c("darkorange", "olivedrab4", "chocolate4",  "cornflowerblue", "grey80", "skyblue3", "violet")
  my_lwd <- c(3, 1.5, 1.25, 1.5, 1.5, 1.25, 1.56)

  lwd_min <- 2
  lty_min <- "dotdash"
  col_min <- my_col[1]
  my_sub <- sprintf("%s; stocks: SP500; bonds: %s", my_sub, BOND_NAME)

  for (year_span in c(1, 16, 20, 26, 31, 37)) {
    growth_20 <- growth_df[growth_df$years == year_span, c(3, 5:10)]
    y_mat <- growth_20[, (2:7)]
    y_mat <- 100 * (exp(log(y_mat) / year_span) - 1)
    if (year_span == 20) {
      prem <-
        data.frame(
          prem_ybar = y_mat[,1] - y_mat[,5],
          prem_min = y_mat[,2] - y_mat[,5],
          prem_mid = y_mat[,3] - y_mat[,5],
          prem_max = y_mat[,4] - y_mat[,5],
          prem_bond = y_mat[,5] - y_mat[,5],
          prem_stock = y_mat[,6] - y_mat[,5]
          )
      svg_path <- img_ext(sprintf("premium_cf%dyr", year_span))
      my_main <- sprintf("Real risk premium over %d-year intervals", year_span)
      my_xlab <-  sprintf("start year for %d-year interval", year_span)
      my_xlim <- c(min(growth_20$date_from), max(growth_20$date_from))
      tryCatch({
        img(svg_path, width = 800/72, height = 425/72)
        matplot(
          main = my_main,
          sub = paste0(my_sub, "; ", parm_coda),
          xlab = my_xlab,
          ylab = "Real CAGR for allocation minus real CAGR for GS10",
          x = growth_20$date_from,
          y = prem[, c(1:4, 6)],
          xlim = my_xlim,
          #ylim = c(min(y_mat), max(y_mat)),
          xaxp = c(1913, 2003, 9),
          yaxp = c(-3, 16, 19),
          las = 1,
          type="l",
          lwd = my_lwd[c(1:4, 7)],
          lty = my_lty[c(1:4, 7)],
          col = my_col[c(1:4, 7)],
          log = ""
          )
        #lines(
        #  x = c(2003, 2003),
        #  y = c(-2, 10),
        #  lty = "solid",
        #  lwd = 0.375,
        #  col = "violet"
        #  )
        legend(
          x = "topright",
          legend = c(
            "min(extended YBAR)",
            "extended YBAR",
            paste0(round(parm_df$stock_lo * 100), ":", round((1 - parm_df$stock_lo) * 100), " SP500:GS10"),
            "bond-only YBAR", # paste0(round(parm_df$mid * 100), "% stock"),
            paste0(round(parm_df$stock_hi * 100), ":", round((1 - parm_df$stock_hi) * 100), " SP500:GS10"),
            # paste0(round(parm_df$stock_lo * 100), "% stock"),
            # "bond-only YBAR", # paste0(round(parm_df$mid * 100), "% stock"),
            # paste0(round(parm_df$stock_hi * 100), "% stock"),
            #"GS10",
            "SP500"
            )[c(1 + my_scramble)][1:5][c(5,1,2,4,3)],
          cex = 0.85,
          lwd = my_lwd[my_scramble][c(1:4, 7)][c(5,1,2,4,3)],
          lty = my_lty[my_scramble][c(1:4, 7)][c(5,1,2,4,3)],
          col = my_col[my_scramble][c(1:4, 7)][c(5,1,2,4,3)]
          )
        }, finally = dev.off()
      )
    } # end if year_span == 20
    svg_path <- img_ext(sprintf("fig1_cf%dyr", year_span))
    my_main <- sprintf("Real compound annual growth rate over %d-year intervals", year_span)
    my_xlab <-  sprintf("start year for %d-year interval", year_span)
    if (year_span == 1) {
      start_year <- 2017
      my_xlim <- with(
        growth_20,
        c(min(date_from[date_from > start_year]), max(date_from))
        )
      my_ylim <- c(
        min(y_mat[growth_20$date_from > start_year, ], na.rm = T),
        max(y_mat[growth_20$date_from > start_year, ], na.rm = T)
        )
      my_xaxp <- c(start_year, 2025, 2025 - start_year)
      my_yaxp <- c(-50, 50, 10)
    } else {
      my_xlim <- c(min(growth_20$date_from), max(growth_20$date_from))
      my_ylim <- c(min(y_mat, na.rm = T), max(y_mat, na.rm = T))
      my_xaxp <- c(1913, 2003, 9)
      my_yaxp <- c(-3, 16, 19)
    }
    tryCatch({
      cat(paste0("writing '", svg_path, "'\n", file = stderr()))
      img(svg_path, width = 800/72, height = 425/72)
      matplot(
        main = my_main,
        sub = paste0(my_sub, "; ", parm_coda),
        xlab = my_xlab,
        ylab = "real CAGR (percent)",
        x = growth_20$date_from,
        y = y_mat,
        xlim = my_xlim,
        ylim = my_ylim,
        xaxp = my_xaxp,
        yaxp = my_yaxp,
        las = 1,
        type="l",
        lwd = my_lwd[c(1:4, 6:7)],
        lty = my_lty[c(1:4, 6:7)],
        col = my_col[c(1:4, 6:7)],
        log = ""
        )
      lines(
        x = c(min(growth_20$date_from), max(growth_20$date_from)),
        y = c(min(y_mat$grow_ybar), min(y_mat$grow_ybar)),
        lty = lty_min,
        lwd = lwd_min,
        col = my_col[1]
        )
      #lines(
      #  x = c(2003, 2003),
      #  y = c(-3, 8.0),
      #  lty = "solid",
      #  lwd = 0.5,
      #  col = "violet"
      #  )
      legend(
        x = "topright",
        legend = c(
          "min(extended YBAR)",
          "extended YBAR",
          paste0(round(parm_df$stock_lo * 100), ":", round((1 - parm_df$stock_lo) * 100), " SP500:GS10"),
          "bond-only YBAR", # paste0(round(parm_df$mid * 100), "% stock"),
          paste0(round(parm_df$stock_hi * 100), ":", round((1 - parm_df$stock_hi) * 100), " SP500:GS10"),
          # paste0(round(parm_df$stock_lo * 100), "% stock"),
          # "bond-only YBAR", # paste0(round(parm_df$mid * 100), "% stock"),
          # paste0(round(parm_df$stock_hi * 100), "% stock"),
          "GS10",
          "SP500"
          )[c(1, 1 + my_scramble)][1:(1 + ncol(y_mat))][c(7,2,3,5,4,6,1)],
        cex = 0.85,
        lwd = c(lwd_min, my_lwd[my_scramble])[no_bills][c(7,2,3,5,4,6,1)],
        lty = c(lty_min, my_lty[my_scramble])[no_bills][c(7,2,3,5,4,6,1)],
        col = c(col_min, my_col[my_scramble])[no_bills][c(7,2,3,5,4,6,1)]
        )
        rel_min_segment_x <- c(1912.458, 1929.708, 1961.708, 1989.208, 2002.875)
        rel_min_segment_y <- c(8,        8,        8,        3,        -1)
        rel_min_label     <- c("A",      "B",      "C",      "D",      "E")
        for (seg_idx in seq_along(rel_min_segment_x)) {
          text(
            x = rel_min_segment_x[seg_idx],
            y = rel_min_segment_y[seg_idx],
            labels = rel_min_label[seg_idx]
            )
        }
      }, finally = dev.off()
    )
  }

  x_mat <- growth_min_df$years
  ycol_minus_1 <- c(1:4,6:7)
  y_mat <- growth_min_df[, 1 + ycol_minus_1]
  y_mat <- 100 * (exp(log(y_mat) / x_mat) - 1)
  tryCatch({
    img(img_ext("min_by_year"), width = 800/72, height = 300/72)
    matplot(
      main = "Minimum compound annual growth rate vs. interval length",
      sub = paste0(my_sub, "; ", parm_coda),
      xlab = "length of interval in years",
      ylab = "minimum real CAGR (percent)",
      x = x_mat,
      y = y_mat,
      xlim = c(6, 50),
      ylim = c(-10, max(y_mat)),
      xaxp = c(0, 50, 10),
      yaxp = c(-15, 5, 10),
      las = 1,
      type="l",
      lwd = my_lwd[ycol_minus_1],
      lty = my_lty[ycol_minus_1],
      col = my_col[ycol_minus_1] #, log = "y"
      )
    lines(
      x = c(0, 50),
      y = c(1, 1),
      lty = "solid",
      lwd = 0.5,
      col = "grey80"
      )
    lines(
      x = c(11, 11),
      y = c(-15, 5),
      lty = "solid",
      lwd = 0.5,
      col = "grey80"
      )
    lines(
      x = c(18, 18),
      y = c(-15, 5),
      lty = "solid",
      lwd = 0.5,
      col = "grey80"
      )
    legend(
      x = "bottomright",
      legend = c(
        "extended YBAR",
        paste0(round(parm_df$stock_lo * 100), ":", round((1 - parm_df$stock_lo) * 100), " SP500:GS10"),
        "bond-only YBAR", # paste0(round(parm_df$mid * 100), "% stock"),
        paste0(round(parm_df$stock_hi * 100), ":", round((1 - parm_df$stock_hi) * 100), " SP500:GS10"),
        "GS10",
        "SP500"
        )[c(6,1,3,4,2,5)],
      cex = 0.8, # 0.85 * 0.8,
      lwd = 1.25 * my_lwd[c(7,1,3,4,2,6)],
      lty = my_lty[c(7,1,3,4,2,6)],
      col = my_col[c(7,1,3,4,2,6)]
      )
    }, finally = dev.off()
  )

  x_mat <- growth_min2max_df$years
  y_mat <- growth_min2max_df[, 2:5]
  tryCatch({
    img(img_ext("min2max_by_year"), width = 800/72, height = 400/72)
    matplot(
      main = "Difference between maximum and minimum fold-change vs. interval length",
      sub = my_sub,
      xlab = "length of interval in years",
      ylab = "Difference between maximum and minimum fold-change in value",
      x = x_mat,
      y = y_mat,
      ylim = c(0.5, max(y_mat)),
      xaxp = c(0, 50, 10),
      las = 1,
      type="l",
      lwd = my_lwd,
      lty = my_lty,
      col = my_col , log = "y"
      )
    legend(
      x = "topleft",
      legend = c(
        "extended YBAR",
        paste0(round(parm_df$stock_lo * 100), ":", round((1 - parm_df$stock_lo) * 100), " SP500:GS10"),
        "bond-only YBAR", # paste0(round(parm_df$mid * 100), "% stock"),
        paste0(round(parm_df$stock_hi * 100), ":", round((1 - parm_df$stock_hi) * 100), " SP500:GS10")
        # paste0(round(parm_df$stock_lo * 100), "% stock"),
        # "bond-only YBAR", # paste0(round(parm_df$mid * 100), "% stock"),
        # paste0(round(parm_df$stock_hi * 100), "% stock")
        ),
      cex = 0.85,
      lwd = my_lwd,
      lty = my_lty,
      col = my_col
      )
    }, finally = dev.off()
  )

  x_mat <- relative_growth_min2max_df$years
  y_mat <- relative_growth_min2max_df[, 2:4]
  tryCatch({
    img(img_ext("rel_min2max_by_year"), width = 800/72, height = 400/72)
    matplot(
      main = "Difference between maximum and minimum fold-change vs. interval length, relative to YBAR",
      sub = my_sub,
      xlab = "length of interval in years",
      ylab = "Relative difference between maximum and minimum fold-change in value",
      x = x_mat,
      y = y_mat,
      #ylim = c(0.08, max(y_mat)),
      xaxp = c(0, 50, 10),
      las = 1,
      type="l",
      lwd = my_lwd[2:4],
      lty = my_lty[2:4],
      col = my_col[2:4],
      log = "y"
      )
    legend(
      x = "bottomleft",
      legend = c(
        paste0(round(parm_df$stock_lo * 100), "% stock"),
        paste0(round(parm_df$mid * 100), "% stock"),
        paste0(round(parm_df$stock_hi * 100), "% stock")
        ),
      cex = 0.85,
      lwd = my_lwd[2:4],
      lty = my_lty[2:4],
      col = my_col[2:4]
      )
    }, finally = dev.off()
  )
  grow20cagr <- (
    growth_df[growth_df$years == 20,] |>
      (function(.) cbind(.[,3,drop = F], exp(log(.[,5:8])/20)-1))()
    ) #; View(grow20cagr)
  sink(file = stderr())
  cat("----------------------------------------------------------------\n")
  cat("scenario: ", my_variant, "\n")
  grow20cagr$ybar_prem <- grow20cagr$grow_ybar - grow20cagr$grow_max
  print(
    with(
      grow20cagr,
      grow20cagr[
        date_from > 1912.375 & date_from < 1912.541 |
          date_from > 1929.625 & date_from < 1929.791 |
          date_from > 1961.625 & date_from < 1961.791 |
          date_from > 1962.125 & date_from < 1962.291 |
          date_from > 1988.792 & date_from < 1988.957 |
          date_from > 1989.125 & date_from < 1989.291 |
          date_from > 1998.875 & date_from < 1999 |
          date_from > 2002.792 & date_from < 2002.957,
        ]
      )
    )
  print(
    sqldf::sqldf(
      "SELECT min(grow_ybar), min(grow_min), min(grow_mid), min(grow_max)
         FROM grow20cagr
      ")
    )
  print(
    sqldf::sqldf(
      "SELECT avg(grow_ybar), avg(grow_min), avg(grow_mid), avg(grow_max)
         FROM grow20cagr
      ")
    )
  print(avg_alloc_df)
  print(avg_alloc_1961_df)
  print(parm_df)
  sink(file = NULL)

  min_factor <- parm_df$min_factor
  max_factor <- parm_df$max_factor
  H <- parm_df$H
  T <- parm_df$T
  X <- 21.46 # TODO compute X from today's date
  cap <- function(s, h) h / pmax(h, 1 / (s * X) - 1)

  x <- (1:801)/4000
  y <- x * 0.75
  
  min_x <- min(x)
  max_x <- 0.09

  f_margin_of_safety <-
    function(s, b) {
      (s / b - 1)
    }
  f_margin_of_folly <-
    function(s, b) {
      (b / s - 1)
    }
  PICO <- 1e-12
  if (my_variant %in% c("e10p_margin", "ep_margin")) {
    f_min_stock_proportion <-
      function(s, b) {
        pmin(
          cap(s, H),
          parm_df$Ma + PICO,
          pmax(
            -PICO,
            (min_factor * b / T) * f_margin_of_safety(s, b)
          )
        )
      }
    f_max_stock_proportion <-
      function(s,b) {
        pmin(
          cap(s, H),
          parm_df$Ma + PICO,
          pmax(
            parm_df$Mi - PICO,
            1 - (max_factor * b / T) * f_margin_of_folly(s, b)
          )
        )
      }
  } else {
    f_min_stock_proportion <-
      function(s, b) {
        pmin(
          cap(s, H),
          parm_df$Ma + PICO,
          pmax(
            -PICO,
            (s - b - 0.0009) / 0.0222
          )
        )
      }
    f_max_stock_proportion <-
      function(s,b) {
        pmin(
          cap(s, H),
          parm_df$Ma + PICO,
          pmax(
            parm_df$Mi - PICO,
            (s - b + 0.0111) / 0.0097
          )
        )
      }
  }

  # ---

    tryCatch({
      img(img_ext("margins_of_safety_and_folly"), width = 650/72, height = 650/72)
      op <- par(mar = c(5,4,7,4) + 0.2)
      contour(
        x,
        y,
        z =
          outer(
            x,
            y,
            f_margin_of_safety
          ),
        levels = c(0, 0.25 * (1:4)),
        main = "Margins of safety and folly",
        xlab = "S&P 500 earnings yield (e.g., E10/P)",
        ylab = "nominal ten-year US Treasury bonds current yield / 100%",
        xlim = c(0, max_x), ylim = c(0, max_x),
        xaxp = c(0, .09, 9),
        yaxp = c(0, .09, 9),
        las = 2,
        labcex = 1.0,
        col = c("black", "red", "red", "red", "red")
      )
      # axis(side = 4, yaxp = c(0, .2, 20) * 0.75, las = 2)
      # axis(side = 3, xaxp = c(0, .2, 20), las = 2)
      grid_four(1)
      grid_four(2)
      ## lines(
      ##   c(min_x, max_x),
      ##   c(parm_df$T, parm_df$T),
      ##   col = "blue",
      ##   lty = "dotted"
      ##   )
      par(new=TRUE)
      op <- par(mar = c(5,4,7,4) + 0.2)
      contour(
        x,
        y,
        z =
          outer(
            x,
            y,
            f_margin_of_folly
          ),
        levels = c(0.25 * (1:4)),
        main = "",
        xlab = "",
        ylab = "",
        xlim = c(0, max_x), ylim = c(0, max_x),
        xaxp = c(0, .09, 9),
        yaxp = c(0, .09, 9),
        axes = FALSE,
        las = 2,
        labcex = 1.0,
        col = "blue",
        lty = "dashed",
        lwd = 1.0
      )
    legend(
      x = "topleft",
      legend = c(
        "margin of safety",
        "margin of folly",
        "historic average GS10 yield"
        ),
      col = c("red", "blue", "grey30"),
      lwd = c(1.0, 1.0, 1.0),
      lty = c("solid", "dashed", "dotted")
      )
    }, finally = {
      dev.off()
      par(op)
    }
    )

    my_sub_prop =
      sprintf(
        "D = %0.2f; T = %0.2f%s; H = %0.2f",
        parm_df$trgr,
        100 * parm_df$T,
        "%",
        parm_df$H
        )
    my_xlim <- c(0.02, 0.1)
    tryCatch({
      img(img_ext("min_max_stock_at_4pt14pct"), width = 650/72, height = 650/72)
      op <- par(mar = c(5,4,5,2) + 0.2)
      plot(
        function(x) f_min_stock_proportion(x, parm_df$T),
        col = "red",
        lty = "solid",
        main = "Minimum and maximum stock proportion vs. earnings yield",
        sub = my_sub_prop,
        xlab = "stock earnings yield",
        ylab = "stock proportion, when nominal bond current yield = 4.14%",
        xaxp = c(0, .1, 10),
        yaxp = c(0, 1, 10),
        xlim = my_xlim,
        ylim = c(0, 1),
        las = 1
        )
      axis(side = 4, yaxp = c(0,   1, 10), las = 1)
      axis(side = 3, xaxp = c(0, 0.1, 10), las = 1)
      # par(xaxp = c(0,1,5), yaxp = c(0,1,5))
      # grid_four(1)
      # grid_four(2)
      par(new=TRUE)
      plot(
        function(x) f_max_stock_proportion(x, parm_df$T),
        col = "blue",
        lty = "dashed",
        axes = FALSE,
        xlab = "",
        ylab = "",
        xlim = my_xlim,
        ylim = c(0, 1)
        )
      par(new=TRUE)
      if (my_variant != "ybar_intro") {
        plot(
          function(x)  f_min_stock_proportion(x, parm_df$T) - parm_df$trgr,
          col = "red",
          lty = "dotted",
          lwd = 1,
          axes = FALSE,
          xlab = "",
          ylab = "",
          xlim = my_xlim,
          ylim = c(0, 1)
          )
        par(new=TRUE)
        plot(
          function(x)  f_max_stock_proportion(x, parm_df$T) + parm_df$trgr,
          col = "blue",
          lty = "dotted",
          lwd = 1,
          axes = FALSE,
          xlab = "",
          ylab = "",
          xlim = my_xlim,
          ylim = c(0, 1)
          )
        legend(
          x = "bottomright",
          legend = c(
            "minimum stock proportion",
            latex2exp::TeX("(minimum stock proportion) - D"),
            "maximum stock proportion",
            latex2exp::TeX("(maximum stock proportion) + D")
            #, "relative reversed rank of E10/P"
            ),
          lty = c("solid", "dotted", "dashed", "dotted", "dotdash"),
          lwd = c(1, 1, 1, 1, 1.25),
          col = c("red", "red", "blue", "blue","violet" )
          )
        text(
          x = 0.0275 + T - 0.0414,
          y = 0.75,
          labels = "region of folly"
          )
        text(
          x = 0.0435 + T - 0.0414,
          y = c(0.55, 0.52, 0.49),
          labels = c("region of", "capital", "appreciation")
          )
        text(
          x = 0.06 + T - 0.0414,
          y = 0.3,
          labels = "region of safety"
          )
      } else { # my_variant == "ybar_intro"
        plot(
          #function(x)  1.8 * f_margin_of_safety(x, parm_df$T),
          function(x)  2.0 * f_margin_of_safety(x, parm_df$T),
          col = "red",
          lty = "dotted",
          lwd = 1,
          axes = FALSE,
          xlab = "",
          ylab = "",
          xlim = my_xlim,
          ylim = c(0, 1)
          )
        par(new=TRUE)
        plot(
          function(x)  1 - 3 * f_margin_of_folly(x, parm_df$T),
          col = "blue",
          lty = "dotted",
          lwd = 1,
          axes = FALSE,
          xlab = "",
          ylab = "",
          xlim = my_xlim,
          ylim = c(0, 1)
          )
        legend(
          x = "right",
          legend = c(
            "minimum stock proportion",
            "maximum stock proportion",
            #latex2exp::TeX("1.8 $\\times$ margin of safety"),
            latex2exp::TeX("2 $\\times$ margin of safety"),
            latex2exp::TeX("1 - 3 $\\times$ margin of folly")
            , "relative reversed rank of E10/P"
            ),
          lty = c("solid", "dashed", "dotted", "dotted", "dotdash"),
          lwd = c(1, 1, 1, 1, 1.25),
          col = c("red", "blue", "red", "blue","violet" )
          )
        par(new=TRUE)
        my_pctile_x <- sort(1 / graham_base_df$pe10)
        my_pctile_y <- 1 - rank(my_pctile_x) / length(my_pctile_x)
        plot(
          x = my_pctile_x,
          y = my_pctile_y,
          col = "violet",
          type = "l",
          pch = ".",
          lty = "dotdash",
          lwd = 1,
          axes = FALSE,
          xlab = "",
          ylab = "",
          xlim = my_xlim,
          ylim = c(0, 1)
          )
      }
    }, finally = {
      dev.off()
      par(op)
    }
    )
    
  # ---

  tryCatch({
    img(img_ext("min_stock_proportion"), width = 650/72, height = 650/72)
    op <- par(mar = c(5,4,7,4) + 0.2)
    contour(
      x,
      y,
      z =
        outer(
          x,
          y,
          f_min_stock_proportion
          ),
      levels = c(0.001, 0.06 * (1:13), parm_df$Ma),
      labels = c(
        "0", "0.06", "0.12", "0.18", "0.24",
        "0.3", "0.36", "0.42", "0.48", "0.54",
        "0.6", "0.66", "0.72", "0.78",
        sprintf("%0.2f", parm_df$Ma)),
      method = c("simple", "edge", "flattest")[3],
      main = "Minimum proportion of stocks",
      sub = sprintf("%s; T = %0.02f%s", parm_coda_H, 100 * parm_df$T, "%"),
      xlab = "S (S&P 500 earnings yield, i.e., E10/P)",
      ylab = "B (bond yield / 100%)",
      xlim = c(0, max_x), ylim = c(0, max_x),
      xaxp = c(0, .09, 9),
      yaxp = c(0, .09, 9),
      las = 2,
      labcex = 0.7,
      col = "red"
      )
    grid_four(1)
    grid_four(2)
    
    lines(
      c(min_x, max_x),
      c(parm_df$T, parm_df$T),
      col = "blue",
      lty = "dotted"
      )
    
    }, finally = {
      dev.off()
      par(op)
    }
  )

  tryCatch({
    img(img_ext("max_stock_proportion"), width = 650/72, height = 650/72)
    op <- par(mar = c(5,4,7,4) + 0.2)
    contour(
      x,
      y,
      z =
        outer(
          x,
          y,
          f_max_stock_proportion
          ),
      levels = c(parm_df$Mi, 0.06 * (2:13), parm_df$Ma),
      labels = c(
        sprintf("%0.2f", parm_df$Mi), "0.12", "0.18", "0.24",
        "0.3", "0.36", "0.42", "0.48", "0.54",
        "0.6", "0.66", "0.72", "0.78",
        sprintf("%0.2f", parm_df$Ma)),
      main = "Maximum proportion of stocks",
      sub = sprintf("%s; T = %0.02f%s", parm_coda_H, 100 * parm_df$T, "%"),
      xlab = "S (S&P 500 earnings yield, i.e., E10/P)",
      ylab = "B (bond yield / 100%)",
      xlim = c(0, max_x), ylim = c(0, max_x),
      xaxp = c(0, .09, 9),
      yaxp = c(0, .09, 9),
      las = 2,
      labcex = 0.7,
      lty = "dashed",
      col = "blue"
      )
    
    grid_four(1)
    grid_four(2)
    
    lines(
      c(min_x, max_x),
      c(parm_df$T, parm_df$T),
      col = "blue",
      lty = "dotted"
    )
    }, finally = {
      dev.off()
      par(op)
    }
  )

  par(mar = c(5, 4, 4, 2) + 0.1)

  my_legend_cex <- 0.83
  my_height <- 750
  yrslice <- c(1,3)
  yrslice <- 1:2
  for (my_yrslice in yrslice) {
    #norm_yrslice <- switch(my_yrslice, 361, 969, 361)
    #norm_yrslice <- switch(my_yrslice, 361, 1389, 969)
    #norm_yrslice <- switch(my_yrslice, 361, 1463, 969)
    norm_yrslice <- switch(my_yrslice, 361, DETAIL_CUTOFF_NORM, 969)
    if (my_yrslice == 1) {
      my_xlim <- c(grow_df$DateFraction[grow_df$seq > norm_yrslice][1], X_MAX)
      #my_xlim <- c(1911, X_MAX)
      #my_xlim <- c(1915, X_MAX)
      #my_xlim <- c(1915, max(grow_df$DateFraction))
      my_log <- "y"
      my_xlab <-
        c(
          "1912", "1922", "1932", "1942", "1952", "1962", "1972", "1982",
          "1992", "2002", "2022", "2032", "2012"
          )
      img(img_ext("value_alloc_yields_1911_2022"), width = 650/72, height = my_height/72)
    } else if (my_yrslice == 2) {
      #my_xlim <- c(1996.75, X_MAX)
      my_xlim <- c(DETAIL_CUTOFF_DATE, X_MAX)
      
      #my_xlim <- c(2000, max(grow_df$DateFraction))
      # my_log <- ""
      my_log <- "y"
      # my_xlab <-
      #   c(
      #     "1960", "1965", "1970", "1975", "1980", "1985", "1990", "1995",
      #     "2000", "2005", "2010", "2015", "2020"
      #     )
      my_xlab <-
        c(
          "1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969",
          "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979",
          "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
          "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
          "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
          "2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"
          )
      img(img_ext("value_alloc_yields_1960_2022"), width = 650/72, height = my_height/72)
    } else {
      my_xlim <- c(1963.75, X_MAX)
      #my_xlim <- c(2000, max(grow_df$DateFraction))
      # my_log <- ""
      my_log <- "y"
      # my_xlab <-
      #   c(
      #     "1960", "1965", "1970", "1975", "1980", "1985", "1990", "1995",
      #     "2000", "2005", "2010", "2015", "2020"
      #     )
      my_xlab <-
        c(
          "1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969",
          "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979",
          "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
          "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
          "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
          "2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"
          )
      img(img_ext("value_alloc_yields_1960_2022"), width = 650/72, height = my_height/72)
    }
    
    my_height_adj <- 4 - 2.1 * (650.0 / my_height)
    nf <- layout(
      mat = matrix(c(1,2,3), 3, 1, byrow = TRUE),
      #heights = c(1.9, 1.05, 1.05)
      heights = c(my_height_adj, 1.05, 1.05)
      )
    #layout.show(nf)
    
    ######## top panel ########
    norm_grow <-
      with(
        grow_df,
        cbind(
          total_ybar / total_ybar[norm_yrslice],
          total_min / total_min[norm_yrslice],
          total_mid / total_mid[norm_yrslice],
          total_max / total_max[norm_yrslice],
          total_tbill30 / total_tbill30[norm_yrslice],
          total_bond / total_bond[norm_yrslice],
          total_stock / total_stock[norm_yrslice]
        )
      )
    
    if (my_yrslice == 1) {
      norm_1911 <- norm_grow
      colnames(norm_1911) <- c("extended_YBAR", "stock_min", "old_YBAR", "stock_max", "money_mkt", "GS10", "SP500")
      norm_1911 <- as.data.frame(norm_1911)
      norm_relative <-
        as.data.frame(norm_1911[13:nrow(norm_1911), ] / norm_grow[1:(nrow(norm_1911) - 12), ])
      norm_1911$Date_Fraction <- grow_df$DateFraction[1:nrow(norm_1911)]
      norm_1911 <- norm_1911[, c(8, 1:7)]
      norm_relative$Date_Fraction <- norm_1911$Date_Fraction[1:nrow(norm_relative)]
      norm_relative <- norm_relative[, c(8, 1:7)]
      
      sr_f <-
        function(choice) {
          date_selector <- choice
          # date_begin <- c(1926, 1911, 1911, 1961, 1961, 1981, 1981, 1991, 1991, 2000, 2000)[date_selector]
          # date_end   <- c(2013, 2022, 2023, 2022, 2023, 2022, 2023, 2022, 2023, 2022, 2023)[date_selector]
          # date_begin <- c(1903, 1911, 1923, 1943, 1963, 1983, 2003)[date_selector]
          # date_end   <- c(1923, 1931, 1943, 1963, 1983, 2003, 2023)[date_selector]
          date_begin <- c(1911, 1911, 1936, 1961, 1991, 1911, 1933, 1955, 1977, 1999)[date_selector]
          date_end   <- c(RECENT_CUTOFF, 1936, 1961, 1993, RECENT_CUTOFF, 1933, 1955, 1977, 1999, RECENT_CUTOFF)[date_selector]
          sr_calc(date_begin, date_end)
        }
      
      geomean <- function(x) exp(mean(log(x)))
      
      # According to:
      #   https://web.stanford.edu/~wfsharpe/art/sr/SR.htm
      #   https://en.wikipedia.org/wiki/Sharpe_ratio#History
      # the 1994 version of the Sharpe ratio is:
      #   mean(d) / sd(d), where d is rate - risk-free rate
      sr_calc <-
        function(date_begin, date_end) {
          date_filter <-
            (norm_relative$Date_Fraction > date_begin) &
              (norm_relative$Date_Fraction < (date_end))
          rslt <-
            with(
              norm_relative[date_filter, ],
              data.frame(
                from_Jan = date_begin,
                thru_Dec = date_end - 1,
                years = date_end - date_begin,
                extended_YBAR =
                  (mean(extended_YBAR - GS10)) / sd(extended_YBAR - GS10),
                old_YBAR =
                  (mean(old_YBAR - GS10)) / sd(old_YBAR - GS10),
                SP500 =
                  (mean(SP500 - GS10)) / sd(SP500 - GS10),
                stock_max =
                  (mean(stock_max - GS10)) / sd(stock_max - GS10),
                stock_min =
                  (mean(stock_min - GS10)) / sd(stock_min - GS10),
                money_mkt =
                  (mean(money_mkt - GS10)) / sd(money_mkt - GS10)
              )
            )
        }
      
      sr_df <- data.frame()
      for (i_sr in 1:5) sr_df <- rbind(sr_df, sr_f(i_sr))
      sr_20_df <- data.frame()
      for (i_sr in 1:93) {
         sr_20_df <- rbind(sr_20_df, sr_calc(1910 + i_sr, 1930 + i_sr))
      }
      sharpe_scramble <- c(1:2, 5, 4, 6, 3)
      sharpe_lwd <- my_lwd[my_scramble][c(1:4, 7)]
      sharpe_lty <- my_lty[my_scramble][c(1:4, 7)]
      sharpe_col <- my_col[my_scramble][c(1:4, 7)]
      # # print(colnames(sr_20_df[, 4:9][, sharpe_scramble]))
      # svg_path <- img_ext("sharpe_20yr")
      # img(svg_path, width = 800/72, height = 400/72)
      # matplot(
      #   x = sr_20_df$from_Jan,
      #   y = sr_20_df[, 4:9][, sharpe_scramble],
      #   type = "l",
      #   xaxp = c(1913, 2003, 9),
      #   main = "Sharpe ratio over 20-year intervals",
      #   xlab = "start year",
      #   ylab = "Sharpe ratio",
      #   lwd = sharpe_lwd,
      #   lty = sharpe_lty,
      #   col = sharpe_col
      #   )
      # legend(
      #   x = "bottomright",
      #   # legend = colnames(sr_20_df[, 4:9][, sharpe_scramble]),
      #   legend = c(
      #     "extended YBAR",
      #     "bond-only YBAR", # paste0(round(parm_df$mid * 100), "% stock"),
      #     paste0(round(parm_df$stock_lo * 100), "% stock"),
      #     paste0(round(parm_df$stock_hi * 100), "% stock"),
      #     "GS10",
      #     "SP500"
      #   ),
      #   cex = my_legend_cex,
      #   lwd = sharpe_lwd,
      #   lty = sharpe_lty,
      #   col = sharpe_col
      #   )
      # dev.off()
      print("Sharpe ratio")
      print(sr_df)
      readODS::write_ods(sr_df, "sharpe_ratios.ods")
    }
    
    my_xticks <- function(y0 = -0.005, y1 = 0.005) {
      for (i in as.numeric(my_xlab)) {
        lines(x = c(i, i), y = c(y0, y1), col = "grey70", lwd = 0.75)
      } 
    }
    my_xaxis <- function() {
      axis(
        side = 1,
        las = 2,
        labels = my_xlab,
        at = as.numeric(my_xlab)
        )
    }
    
    if (my_yrslice == 1) {
      my_ylim <- c(0.7, max(norm_grow, na.rm = T))
      my_labels <- c("0.001", "0.01", "0.1", "1", "10", "100", "1000", "10000")
      my_at <- as.numeric(my_labels)
      my_ylab <- "real growth relative to January 1911"
      my_yaxp <- par("yaxp")
    } else if (my_yrslice == 2) {
      subnorm <- norm_grow[grow_df$DateFraction > my_xlim[1], ]
      my_ylim <- c(min(subnorm, na.rm = T), max(subnorm, na.rm = T))
      my_labels <- c("0.1", "0.5", "1", "5", "10", "15", "20", "25", "30", "35", "40")
      my_labels <- c("0.2", "0.4", "0.6", "0.8", "1", "1.2", "1.4", "1.6", "1.8", "2")
      my_at <- as.numeric(my_labels)
      subnorm2 <- norm_grow[grow_df$DateFraction > 2022 & grow_df$DateFraction < 2023, ]
      colnames(subnorm2) <- c("extended_ybar", "stock_min", "old_ybar", "stock_max", "money_mkt", "GS10", "SP500")
      for (. in 1:7) {
        print(
          sprintf(
            "%s decline in 2022 = %0.2f%s",
            colnames(subnorm2)[.],
            100 * (max(subnorm2[, .]) - min(subnorm2[, .])) / max(subnorm2[, .]),
            "%"
            )
          )
        }
      my_ylab <- sprintf(
        "real growth relative to September %d",
        floor(DETAIL_CUTOFF_DATE)
        )
      my_yaxp <- par("yaxp")
      #my_yaxp <- c(0, 2, 10)
    } else {
      my_ylim <- c(0.5, 1.8)
      my_labels <- c("0.2", "0.4", "0.6", "0.8", "1.0", "1.2", "1.5", "1.7")
      my_at <- as.numeric(my_labels)
      my_yaxp <- par("yaxp")
    }
    old_cex <- par(cex = 0.90)
    my_mar <- par("mar")
    my_mar[1] <- 3.1
    old_mar <- par(mar = my_mar)
    my_main <-
      if (my_variant == "ep_margin") {
        "Relative cumulative value (stock earnings yield = E/P)"
      } else {
        "Relative cumulative value"
      }
    x_plot <- grow_df$DateFraction
    y_plot <- norm_grow
    #if (my_yrslice == 2) {
    if (TRUE) {
      y_plot <- norm_grow[grow_df$DateFraction > my_xlim[1], ]
      x_plot <- grow_df$DateFraction[grow_df$DateFraction > my_xlim[1]]
    }
    matplot(
      x = x_plot,
      y = y_plot,
      type = "l",
      log = my_log,
      lwd = my_lwd,
      lty = my_lty,
      col = my_col,
      xlim = my_xlim,
      ylim = my_ylim,
      xlab = "", # "date",
      ylab = my_ylab,
      #yaxp = my_yaxp,
      axes = FALSE, #yaxp = c(.001, 100, 1),
      main = my_main #, sub = sprintf("bonds: %s", BOND_NAME)
      )
    my_xaxis()
    axis(
      side = 2,
      #yaxp = c(.001, 100, 1),
      las = 2,
      labels = my_labels,
      at = my_at
      )
    legend(
      x = DETAIL_LEGEND_TOP,
      legend = c(
        "extended YBAR",
        paste0(round(parm_df$stock_lo * 100), ":", round((1 - parm_df$stock_lo) * 100), " SP500:GS10"),
        "bond-only YBAR", # paste0(round(parm_df$mid * 100), "% stock"),
        paste0(round(parm_df$stock_hi * 100), ":", round((1 - parm_df$stock_hi) * 100), " SP500:GS10"),
        "30 day T-bills",
        "GS10",
        "SP500"
        )[my_scramble][c(7,1,2,4,3,6,5)],
      cex = my_legend_cex,
      lwd = my_lwd[my_scramble][c(7,1,2,4,3,6,5)],
      lty = my_lty[my_scramble][c(7,1,2,4,3,6,5)],
      col = my_col[my_scramble][c(7,1,2,4,3,6,5)]
      )
    if (my_yrslice == 1) {
      rel_min_segment_x <- c(1912.458, 1929.708, 1961.708, 1989.208, 2002.875)
      rel_min_segment_y <- c(10,       20,       100,      800,      1700)
      rel_min_label     <- c("A",      "B",      "C",      "D",      "E")
    } else if (my_yrslice == 2) {
      rel_min_segment_x <- c(1961.708,  2002.875)
      rel_min_segment_y <- c(6,         10)
      rel_min_label     <- c("C",       "E")
    } else {
      rel_min_segment_y <- c(4, 20, 50)
      rel_min_segment_x <- rel_min_segment_x[3:5]
      rel_min_label <- rel_min_label[3:5]
    }
    for (seg_idx in seq_along(rel_min_segment_x)) {
        lines(
          x = c(0, 20) + rel_min_segment_x[seg_idx],
          y = c(0, 0) + rel_min_segment_y[seg_idx]
          )
        text(
          x = 10 + rel_min_segment_x[seg_idx],
          y = rel_min_segment_y[seg_idx] / 1.2,
          labels = rel_min_label[seg_idx]
          )
    }
    
    # mar for middle and bottom panels
    
    my_mar[1] <- 0.1
    my_mar <- par(mar = my_mar)
    
    ######## middle panel ########
    equity_premium <- grow_df$S - grow_df$B - VCLT_PRM / 100
    if (my_yrslice == 1) {
      my_ylim <- c(min(equity_premium) - 0.02, max(equity_premium))
      my_ylabels <- c("-4", "0", "4", "8", "12", "16")
      my_purple_legend <- "top"
    } else if (my_yrslice == 2) {
      # my_ylim <- c(min(equity_premium), max(equity_premium))
      my_ylim <- c(-0.05, 0.08)
      my_ylabels <- c("-4", "-2", "0", "2", "4", "6")
      my_ylabels <- c("-4", "-2", "0", "2", "4", "6", "8", "10", "12", "14", "16")
      my_purple_legend <- DETAIL_LEGEND_MIDDLE
    } else {
      my_ylim <- c(-0.04, 0.06)
      my_ylabels <- c("-3", "-2", "-1", "0", "1", "2", "3", "4", "5")
      my_purple_legend <- "bottom"
    }
    if (TRUE) {
      y_plot <- grow_df[grow_df$DateFraction > my_xlim[1], ]$B
      y_plot2 <- with(grow_df[grow_df$DateFraction > my_xlim[1], ], S - B)
      x_plot <- grow_df[grow_df$DateFraction > my_xlim[1], ]$DateFraction
    }
    matplot(
      #x = grow_df$DateFraction,
      #y = grow_df$B,
      x = x_plot,
      y = y_plot,
      type = "l",
      lty = "12",
      col = "blue",
      xlim = my_xlim,
      ylim = my_ylim,
      xlab = "",
      ylab = "",
      axes = FALSE
      )
    par(new=TRUE)
    matplot(
      #x = grow_df$DateFraction,
      #y = grow_df$S - grow_df$B,
      x = x_plot,
      y = y_plot2,
      type = "l",
      lty = "solid",
      col = "red",
      lwd = 0.75, # 1.25,
      xlim = my_xlim,
      ylim = my_ylim,
      xlab = "",
      ylab = "percent",
      axes = FALSE, #yaxp = c(.001, 100, 1),
      main = "Nominal GS10 yield and SP500 yield premium"
      )
    # my_xaxis()
    axis(
      side = 2,
      #yaxp = c(.001, 100, 1),
      las = 2,
      labels = my_ylabels,
      at = as.numeric(my_ylabels) / 100
      )
    lines(my_xlim + c(-10, 0), c(0, 0), col = "grey70", lty = "solid", lwd = 0.15)
    my_xticks(y0 = -0.005, y1 = 0.005)
    legend(
      x = my_purple_legend,
      legend = c(
        "GS10",
        "SP500 - GS10"
        ),
      cex = 0.85,
      lwd = c(1, 0.75), # c(1, 1.25),
      lty = c("12", "solid"),
      col = c("blue", "red"),
      bg="white"
      )
    
    ######## bottom panel ########
    y_plot <- with(grow_df[grow_df$DateFraction > my_xlim[1], ], alloc)
    x_plot <- grow_df[grow_df$DateFraction > my_xlim[1], ]$DateFraction
    matplot(
      #x = grow_df$DateFraction,
      #y = grow_df$alloc,
      x = x_plot,
      y = y_plot,
      type = "l",
      lty = "11",
      col = "grey50",
      lwd = 1.5,
      xlim = my_xlim,
      ylim = c(0, 1),
      xlab = "", # "date",
      ylab = "% stock/\"cash\" allocation",
      axes = FALSE, #yaxp = c(.001, 100, 1),
      sub = paste0(my_sub, "; ", parm_coda),
      main = "Extended YBAR percentage stock/\"cash\" allocation"
      )
    my_filtp <- (1 - grow_df$alloc) * grow_df$fxdinc_lngtrm_prprtn
    my_alloc <- grow_df$alloc
    my_filtp <- 1 - my_alloc - my_filtp
    my_alloc <- ifelse(grow_df$trade == "", NA, my_alloc)
    my_buy <- ifelse(grow_df$trade == "+", my_alloc, NA)
    my_sell <- ifelse(grow_df$trade == "-", my_alloc, NA)
    my_sellbond <- ifelse(grow_df$fixtrade == "-", my_filtp, NA)
    my_buybond <- ifelse(grow_df$fixtrade == "+", my_filtp, NA)
    my_ylabels <- c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100")
    axis(
      side = 2,
      #yaxp = c(.001, 100, 1),
      las = 2,
      labels = my_ylabels,
      at = as.numeric(my_ylabels) / 100
      )
    par(new=TRUE)
    matplot(
      #x = grow_df$DateFraction,
      #y = my_buy,
      x = x_plot,
      y = my_buy[grow_df$DateFraction > my_xlim[1] ],
      type = "p",
      pch = 24,
      lty = "solid",
      col = "red",
      bg = "red",
      cex = 1,
      xlim = my_xlim,
      ylim = c(0, 1),
      xlab = "",
      ylab = "",
      axes = FALSE
      )
    par(new=TRUE)
    matplot(
      x = x_plot,
      y = my_sell[grow_df$DateFraction > my_xlim[1] ],
      type = "p",
      pch = 25,
      lty = c("solid"),
      col = c("blue"),
      cex = 1,
      lwd = 1.25,
      xlim = my_xlim,
      ylim = c(0, 1),
      xlab = "",
      ylab = "",
      axes = FALSE
      )
    lines(my_xlim + c(-10, 0), c(0, 0), col = "grey70", lty = "solid", lwd = 0.15)
    par(new=TRUE)
    matplot(
      x = x_plot,
      y = my_filtp[grow_df$DateFraction > my_xlim[1] ],
      type = "l",
      lwd = 1.25,
      lty = "solid",
      col = "violet",
      xlim = my_xlim,
      ylim = c(0, 1),
      xlab = "",
      ylab = "",
      axes = FALSE
      )
    par(new=TRUE)
    matplot(
      x = x_plot,
      y = my_sellbond[grow_df$DateFraction > my_xlim[1] ],
      type = "p",
      pch = 21,
      lty = c("solid"),
      col = c("purple3"),
      cex = 0.625,
      lwd = 1.25,
      xlim = my_xlim,
      ylim = c(0, 1),
      xlab = "",
      ylab = "",
      axes = FALSE
      )
    par(new=TRUE)
    matplot(
      x = x_plot,
      y = my_buybond[grow_df$DateFraction > my_xlim[1] ],
      type = "p",
      pch = 20,
      lty = c("solid"),
      col = c("brown"),
      cex = 0.75,
      lwd = 1.25,
      xlim = my_xlim,
      ylim = c(0, 1),
      xlab = "",
      ylab = "",
      axes = FALSE
      )
    legend(
      x = (
        if (my_yrslice == 1) 1985 else if (my_yrslice == 2) DETAIL_LEGEND_BOTTOM else "top"
        ) ,
      y = (if (my_yrslice == 1) 1 else NULL) ,
      legend = c(
        "% stock",
        "% \"cash\"",
        "sell stock",
        "buy stock",
        "increase \"cash\"",
        "increase bonds"
        ),
      cex = 0.9 * my_legend_cex,
      pt.cex = c(rep.int(my_legend_cex, 4), 0.75, 1),
      #lwd = my_lwd,
      pch = c(NA, NA, 25, 24, 21, 20),
      lty = c("11", "solid", "blank", "blank", "blank", "blank"),
      col = c("grey50", "violet", "blue", "red", "purple3", "brown"),
      pt.bg = c("white", "white", "white", "red", "white", "brown"),
      lwd = c(1, 0.75, 1, 1, 0.75, 0.75)
      )
    my_xticks(y0 = -0.025, y1 = 0.025)
   
    par(old_mar)
    par(old_cex)
    dev.off()
  }
  
  svg_path <- img_ext("sharpe_20yr")
  img(svg_path, width = 800/72, height = 400/72)
  matplot(
    x = sr_20_df$from_Jan,
    y = sr_20_df[, c(4:9)][, sharpe_scramble][c(1:4, 6)],
    type = "l",
    xaxp = c(1913, 2003, 9),
    main = "Sharpe ratio of real returns over 20-year intervals, relative to GS10 real return",
    xlab = "start year",
    ylab = "Sharpe ratio",
    log = "y",
    lwd = sharpe_lwd,
    lty = sharpe_lty,
    col = sharpe_col
    )
  legend(
    x = "bottom",
    # legend = colnames(sr_20_df[, 4:9][, sharpe_scramble]),
    legend = c(
      "extended YBAR",
      "bond-only YBAR", # paste0(round(parm_df$mid * 100), "% stock"),
      paste0(round(parm_df$stock_lo * 100), ":", round((1 - parm_df$stock_lo) * 100), " SP500:GS10"),
      paste0(round(parm_df$stock_hi * 100), ":", round((1 - parm_df$stock_hi) * 100), " SP500:GS10"),
      # paste0(round(parm_df$stock_lo * 100), "% stock"),
      # paste0(round(parm_df$stock_hi * 100), "% stock"),
      # "money market",
      "SP500"
    )[c(5,1,2,4,3)],
    cex = my_legend_cex,
    lwd = sharpe_lwd[c(5,1,2,4,3)],
    lty = sharpe_lty[c(5,1,2,4,3)],
    col = sharpe_col[c(5,1,2,4,3)]
    )
  dev.off()  
  
  if (my_variant %in% c("e10p_margin", "ep", "ybar_intro")) {
    cor_df <-
      sqldf::sqldf(
        "
        SELECT date_from, S, grow_stock, B, grow_bond
        FROM growth_df
        WHERE years = 10
        ORDER BY date_from
        ")
    cor_df$S <- with(cor_df, S / max(S))
    cor_df$B <- with(cor_df, B / max(B))
    cor_df$grow_stock <- with(cor_df, grow_stock / max(grow_stock))
    cor_df$grow_bond <- with(cor_df, grow_bond / max(grow_bond))
    
    cor_stock <-
      sprintf("Pearson correlation = %0.2f", with(cor_df, cor(S, grow_stock)))
    svg_path <- img_ext("fig7_E10P_yield_return_corr")
    img(svg_path, width = 800/72, height = 400/72)
    matplot(
      type = "l",
      x = cor_df$date_from,
      y = cor_df[, c("S", "grow_stock")],
      ylab = "relative earnings yield or return",
      main = "Stock earnings yield and subsequent ten-year return",
      xlab = cor_stock
      )
    legend(
      x = "topright",
      legend = c("stock earnings yield", "real 10-year total return"),
      lty = 1:2,
      col = c("black", "red")
    )
    dev.off()
    
    cor_bond <-
      sprintf("Pearson correlation = %0.2f", with(cor_df, cor(B, grow_bond)))
    svg_path <- img_ext("fig6_bond_yield_return_corr")
    img(svg_path, width = 800/72, height = 400/72)
    matplot(
      type = "l",
      x = cor_df$date_from,
      y = cor_df[, c("B", "grow_bond")],
      ylab = "relative current yield or return",
      main = "Bond current yield and subsequent ten-year return",
      xlab = cor_bond,
      col = c("black", "blue")
      )
    legend(
      x = "topleft",
      legend = c("bond current yield", "real 10-year total return"),
      lty = 1:2,
      col = c("black", "blue")
    )
    dev.off()
  }
  

  switch(
    my_variant,
    ybar_intro = {
      # "fig3_ybar_history.svg"
      # "fig6_bond_yield_return_corr.svg"
      # "fig7_E10P_yield_return_corr.svg"
      # "fig8_EP_yield_return_corr.svg"
      cp_old("fig10_PE10_trend.svg", "fig10_PE10_trend.svg")
      cp_old("fig1_cf16yr.svg", "fig1_cf16yr.svg")
      cp_old("fig1_cf20yr.svg", "fig1_cf20yr.svg")
      cp_old("fig1_cf26yr.svg", "fig1_cf26yr.svg")
      cp_old("fig1_cf31yr.svg", "fig1_cf31yr.svg")
      cp_old("fig1_cf37yr.svg", "fig1_cf37yr.svg")
      cp_old("min_max_stock_at_4pt14pct.svg", "fig2_boundaries.svg")
      cp_old("value_alloc_yields_1911_2022.svg", "fig4_cumulative_all.svg")
      cp_old("value_alloc_yields_1960_2022.svg", "fig5_cumulative_detail.svg")
      #cp_old("value_alloc_yields_1961_1981.svg", "fig5_cumulative_detail.svg")
      cp_old("min_by_year.svg", "fig5b_min_by_year.svg")
      cp_old("fig6_bond_yield_return_corr.svg", "fig6_bond_yield_return_corr.svg")
      cp_old("fig7_E10P_yield_return_corr.svg", "fig7_E10P_yield_return_corr.svg")
      },
    e10p = {
      cp_img("fig1_cf16yr.pdf", "fig1_cf16yr.pdf")
      cp_img("fig1_cf20yr.pdf", "fig1_cf20yr.pdf")
      cp_img("fig1_cf26yr.pdf", "fig1_cf26yr.pdf")
      cp_img("fig1_cf31yr.pdf", "fig1_cf31yr.pdf")
      cp_img("fig1_cf37yr.pdf", "fig1_cf37yr.pdf")
      cp_img("min_max_stock_at_4pt14pct.pdf", "fig2_boundaries.pdf")
      cp_img("min_by_year.pdf", "fig5b_min_by_year.pdf")
      cp_img("fig10_PE10_trend.pdf", "fig10_PE10_trend.pdf")
      },
    ep = {
      cp_old("fig1_cf20yr.svg", "fig1_cf20yr_ep.svg")
      cp_old("min_by_year.svg", "fig5c_min_by_year_pe.svg")
      cp_img("fig1_cf20yr.pdf", "fig1_cf20yr_ep.pdf")
      cp_img("min_by_year.pdf", "fig5c_min_by_year_pe.pdf")
      cp_old("fig7_E10P_yield_return_corr.svg", "fig8_EP_yield_return_corr.svg")
      },
    e10p_margin = {
      cp_img("fig1_cf20yr.pdf", "fig1_cf20yr_mos.pdf")
      cp_img("min_max_stock_at_4pt14pct.pdf", "fig2_boundaries_mos.pdf")
      cp_img("margins_of_safety_and_folly.pdf", "margins_of_safety_and_folly.pdf")
      cp_img("min_stock_proportion.pdf", "min_stock_proportion_mos.pdf")
      cp_img("max_stock_proportion.pdf", "max_stock_proportion_mos.pdf")
      cp_img("min_by_year.pdf", "fig5b_min_by_year_margin.pdf")
      cp_img("fig6_bond_yield_return_corr.pdf", "fig6_bond_yield_return_corr.pdf")
      cp_img("fig7_E10P_yield_return_corr.pdf", "fig7_E10P_yield_return_corr.pdf")
      cp_img("value_alloc_yields_1911_2022.pdf", "fig4_cumulative_all.pdf")
      cp_img("value_alloc_yields_1960_2022.pdf", "fig5_cumulative_detail.pdf")
      #cp_img("value_alloc_yields_1961_1981.pdf", "fig5_cumulative_detail.pdf")
      cp_old("fig1_cf20yr.svg", "fig1_cf20yr_mos.svg")
      cp_img("sharpe_20yr.pdf", "sharpe_20yr.pdf")
      cat(sprintf("copying to dir %s\n", img_dir), file = stderr())
      cp_img("premium_cf20yr.pdf", "premium_cf20yr.pdf")
      },
    ep_margin = {
      cp_img("fig1_cf20yr.pdf", "fig1_cf20yr_ep_mos.pdf")
      cp_img("value_alloc_yields_1911_2022.pdf", "fig4_cumulative_all_ep_mos.pdf")
    }
    )
}
