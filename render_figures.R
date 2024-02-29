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
  my_variant_selector <- c(4)
}

if (!dir.exists("img")) { dir.create("img")}
cp_img <-
  function(src, dst) {
    if(file.exists(src)) {
      file.copy(from = src, to = paste0("img/", dst), overwrite = TRUE)
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

for_variants <- my_variants[my_variant_selector]
for (my_variant in for_variants) {
  if ((my_variant %in% c("ep", "ybar_intro")) || length(my_variant_selector) > 1) {
    img_ext <- function(s) paste0(s, ".svg")
    img <- svg
  } else {
    img_ext <- function(s) paste0(s, ".pdf")
    img <- pdf
  }
  # see notes 7 and 8 of YBAR_intro.html for an explanation of the parameters set here.
  update_sql <-
    switch (
      my_variant,
      e10p_margin = 
        ("
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
              scheme_S = 'e10p', -- 'e10p' use E10/P for stock earnings yield (S)
              scheme_M = 'margin', -- 'margin' use margins of safety and folly for threshold-setting
              min_factor = 2, -- slope-accelerator for min stock percentage (2 works well since 1911)
              max_factor = 3 -- slope-accelerator for max stock percentage (3 works well since 1911)
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
        ("
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
              max_factor = 3 -- slope-accelerator for max stock
              WHERE grp = 1
        ")
    )

  my_dbname <- "graham.sqlite"
  my_parm_f <-
    function(.) {
    my_conn <- RSQLite::dbConnect(RSQLite::SQLite(), my_dbname)
    on.exit(RSQLite::dbDisconnect(my_conn))
    RSQLite::dbExecute(my_conn, update_sql)
  }

  my_dbname |> my_parm_f()

  my_sub <- switch(
    my_variant,
    ybar_intro = "stock earnings yield = E10 / P",
    e10p = "stock earnings yield = E10 / P",
    ep = "stock earnings yield = E / P",
    e10p_margin = "margin-based; stock earnings yield = E10 / P",
    ep_margin = "margin-based; stock earnings yield = E / P"
    )


  graham_base_df <-
    sqldf::sqldf(
      "select * from graham_base",
      dbname = my_dbname
      )
  
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

  growth_df <-
    sqldf::sqldf(
      "select * from growth",
      dbname = my_dbname
    )

  grow_df <-
    sqldf::sqldf(
      "select * from grow",
      dbname = my_dbname
    )

  avg_alloc_df <-
    sqldf::sqldf(
      "SELECT avg(alloc) AS 'Average allocation since 1911'
        FROM grow
        WHERE DateFraction > 1911;",
      dbname = my_dbname
    )

  parm_df <-
    sqldf::sqldf(
      "select * from parm",
      dbname = my_dbname
    )

  parm_coda <-
    ifelse(
      my_variant %in% c("e10p_margin", "ep_margin"),
      sprintf("T = %0.4f; H = %0.2f", parm_df$T, parm_df$H),
      sprintf("H = %0.2f", parm_df$H)
      )
  #parm_coda <-  sprintf("H = %0.2f", parm_df$H)

  my_lwd <- c(3, 1.5, 3, 1.5)
  my_lty <- c("solid",      "dotdash",    "twodash",         "dashed")
  # Line types can either be specified as an integer
  #  (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash)
  #            "solid"            "44"      "13"      "1314"     "73"        "2262"
  # or as one of the character strings
  #  "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash", where "blank" uses ‘invisible lines’ (i.e., does not draw them).

  my_lty <- c("solid",      "dotted",    "twodash",         "dashed")
  my_col <- c("darkorange", "chocolate4", "cornflowerblue",  "olivedrab4")
  lwd_min <- 2
  lty_min <- "dotdash"
  col_min <- my_col[1]

  for (year_span in c(16, 20, 26, 31, 37)) {
    growth_20 <- growth_df[growth_df$years == year_span, c(3, 5:8)]
    y_mat <- growth_20[, 2:5]
    y_mat <- 100 * (exp(log(y_mat) / year_span) - 1)
    svg_path <- img_ext(sprintf("fig1_cf%dyr", year_span))
    my_main <- sprintf("Total return over %d-year intervals", year_span)
    my_xlab <-  sprintf("start year for %d-year interval", year_span)
    #my_xlim <- c(1911, max(growth_20$date_from))
    my_xlim <- c(min(growth_20$date_from), max(growth_20$date_from))
    tryCatch({
      #svg(svg_path, width = 800/72, height = 495/72)
      img(svg_path, width = 800/72, height = 400/72)
      matplot(
        main = my_main,
        sub = paste0(my_sub, "; ", parm_coda),
        xlab = my_xlab,
        ylab = "compound annualized real growth rate (percent)",
        x = growth_20$date_from,
        y = y_mat,
        xlim = my_xlim,
        ylim = c(min(y_mat), max(y_mat)),
        xaxp = c(1910, 2000, 10),
        yaxp = c(-3, 16, 19),
        las = 1,
        type="l",
        lwd = my_lwd,
        lty = my_lty,
        col = my_col,
        log = ""
        )
      lines(
        x = c(min(growth_20$date_from), max(growth_20$date_from)),
        y = c(min(y_mat$grow_ybar), min(y_mat$grow_ybar)),
        lty = lty_min,
        lwd = lwd_min,
        col = my_col[1]
        )
      legend(
        x = "bottomright",
        legend = c(
          "min(YBAR)",
          "YBAR",
          paste0(round(parm_df$Mi * 100), "% stock"),
          paste0(round(parm_df$mid * 100), "% stock"),
          paste0(round(parm_df$Ma * 100), "% stock")
          ),
        cex = 0.85,
        lwd = c(lwd_min, my_lwd),
        lty = c(lty_min, my_lty),
        col = c(col_min, my_col)
        )
        # > growth_min_df[,2:5] |> (function(.,y) exp(log(.[y,])/y))(20)
      }, finally = dev.off()
    )
  }

  x_mat <- growth_min_df$years
  y_mat <- growth_min_df[, 2:5]
  y_mat <- 100 * (exp(log(y_mat) / x_mat) - 1)
  tryCatch({
    #svg("min_by_year.svg", width = 800/72, height = 495/72)
    img(img_ext("min_by_year"), width = 800/72, height = 400/72)
    matplot(
      main = "Minimum annualized growth rate vs. interval length",
      sub = my_sub,
      xlab = "length of interval in years",
      ylab = "minimum compound annualized growth rate (percent) over interval",
      x = x_mat,
      y = y_mat,
      ylim = c(-15, max(y_mat)),
      xaxp = c(0, 50, 10),
      las = 1,
      type="l",
      lwd = my_lwd,
      lty = my_lty,
      col = my_col #, log = "y"
      )
    legend(
      x = "bottomright",
      legend = c(
        "YBAR",
        paste0(round(parm_df$Mi * 100), "% stock"),
        paste0(round(parm_df$mid * 100), "% stock"),
        paste0(round(parm_df$Ma * 100), "% stock")
        ),
      cex = 0.85,
      lwd = my_lwd,
      lty = my_lty,
      col = my_col
      )
    }, finally = dev.off()
  )

  x_mat <- growth_min2max_df$years
  y_mat <- growth_min2max_df[, 2:5]
  tryCatch({
    #svg("min2max_by_year.svg", width = 800/72, height = 495/72)
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
        "YBAR",
        paste0(round(parm_df$Mi * 100), "% stock"),
        paste0(round(parm_df$mid * 100), "% stock"),
        paste0(round(parm_df$Ma * 100), "% stock")
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
    #svg("rel_min2max_by_year.svg", width = 800/72, height = 495/72)
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
        paste0(round(parm_df$Mi * 100), "% stock"),
        paste0(round(parm_df$mid * 100), "% stock"),
        paste0(round(parm_df$Ma * 100), "% stock")
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
          date_from > 1998.875 & date_from < 1999,
        ]
      )
    )
  print(
    sqldf::sqldf(
      "SELECT min(grow_ybar), min(grow_min), min(grow_mid), min(grow_max)
        FROM grow20cagr
      ")
    )
  print(avg_alloc_df)
  print(parm_df)
  sink(file = NULL)

  # View(with(grow20cagr, grow20cagr[grow_ybar == min(grow_ybar) | grow_min == min(grow_min) | grow_mid == min(grow_mid) | grow_max == min(grow_max),]))
  # grow20 <- ( growth_df[growth_df$years == 20,] |> (function(.) cbind(.[,3,drop = F], .[,5:8]))()); View(grow20)

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
      axis(side = 4, yaxp = c(0, .2, 20) * 0.75, las = 2)
      axis(side = 3, xaxp = c(0, .2, 20), las = 2)
      lines(c(min_x, max_x), c(parm_df$T, parm_df$T), col = "blue", lty = "dotted")
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
      x = "bottomright",
      legend = c(
        "margin of safety",
        "margin of folly"
        ),
      col = c("red", "blue"),
      lwd = c(1, 1.0),
      lty = c("solid", "dashed")
      )
    }, finally = {
      dev.off()
      par(op)
    }
    )

    if (my_variant != "ybar_intro") {
      my_sub = sprintf("D = %0.2f", parm_df$trgr)
    } else {
      my_sub = ""
    }
    my_xlim <- c(0.02, 0.1)
    tryCatch({
      img(img_ext("min_max_stock_at_4pt14pct"), width = 650/72, height = 650/72)
      op <- par(mar = c(5,4,5,2) + 0.2)
      plot(
        function(x) f_min_stock_proportion(x, parm_df$T),
        col = "red",
        lty = "solid",
        main = "Minimum and maximum stock proportion vs. earnings yield",
        sub = my_sub,
        xlab = "stock earnings yield",
        ylab = "stock proportion, when nominal bond current yield = 4.14%",
        xaxp = c(0, .1, 10),
        yaxp = c(0, 1, 10),
        xlim = my_xlim,
        ylim = c(0, 1),
        las = 1
        )
      # axis(side = 4, yaxp = c(0,   1, 10), las = 1)
      # axis(side = 3, xaxp = c(0, 0.1, 10), las = 1)
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
          col = c("red", "red", "blue", "blue","purple" )
          )
        text(
          x = 0.0275,
          y = 0.75,
          labels = "region of folly"
          )
        text(
          x = 0.0435,
          y = c(0.55, 0.52, 0.49),
          labels = c("region of", "capital", "appreciation")
          )
        text(
          x = 0.06,
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
          col = c("red", "blue", "red", "blue","purple" )
          )
        par(new=TRUE)
        my_pctile_x <- sort(1 / graham_base_df$pe10)
        my_pctile_y <- 1 - rank(my_pctile_x) / length(my_pctile_x)
        plot(
          x = my_pctile_x,
          y = my_pctile_y,
          col = "purple",
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
      levels = c(0.001, 0.25 * (1:3), parm_df$Ma),
      labels = c("0", "0.25", "0.5", "0.75", sprintf("%0.2f", parm_df$Ma)),
      method = c("simple", "edge", "flattest")[3],
      main = "Minimum proportion of stocks",
      sub = parm_coda,
      xlab = "S (S&P 500 earnings yield, i.e., E10/P)",
      ylab = "B (Ten-year US Treasury bonds current yield / 100%)",
      xlim = c(0, max_x), ylim = c(0, max_x),
      xaxp = c(0, .09, 9),
      yaxp = c(0, .09, 9),
      las = 2,
      labcex = 1.0,
      col = "red"
      )
    axis(side = 4, yaxp = c(0, .2, 20) * 0.75, las = 2)
    axis(side = 3, xaxp = c(0, .2, 20), las = 2)
    
    lines(c(min_x, max_x), c(parm_df$T, parm_df$T), col = "blue", lty = "dotted")
    
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
      levels = c(parm_df$Mi, 0.25 * (1:3), parm_df$Ma),
      #levels = c(0, 0.25 * (1:3), parm_df$Ma),
      main = "Maximum proportion of stocks",
      sub = parm_coda,
      xlab = "S (S&P 500 earnings yield, i.e., E10/P)",
      ylab = "B (Ten-year US Treasury bonds current yield / 100%)",
      xlim = c(0, max_x), ylim = c(0, max_x),
      xaxp = c(0, .09, 9),
      yaxp = c(0, .09, 9),
      las = 2,
      labcex = 1.0,
      lty = "dashed",
      col = "blue"
      )
    
    axis(side = 4, yaxp = c(0, .2, 20) * 0.75, las = 2)
    axis(side = 3, xaxp = c(0, .2, 20), las = 2)
    lines(c(min_x, max_x), c(parm_df$T, parm_df$T), col = "blue", lty = "dotted")
    }, finally = {
      dev.off()
      par(op)
    }
  )

  # tryCatch({
  #   svg(svg_path, width = 495/72, height = 495/72)
  #   }, finally = dev.off()
  # )

  par(mar = c(5, 4, 4, 2) + 0.1)

  my_legend_cex <- 0.83
  my_height <- 750
  for (i in 1:2) {
    if (i == 1) {
      my_xlim <- c(1910, max(grow_df$DateFraction))
      my_xlab <-
        c(
          "1910", "1920", "1930", "1940", "1950", "1960", "1970", "1980",
          "1990", "2000", "2020", "2030", "2010"
          )
      img(img_ext("value_alloc_yields_1911_2022"), width = 650/72, height = my_height/72)
    } else {
      my_xlim <- c(1962, 1981.5)
      my_xlab <-
        c(
          "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968",
          "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976",
          "1977", "1978", "1979", "1980", "1981", "1982"
          )
      img(img_ext("value_alloc_yields_1961_1981"), width = 650/72, height = my_height/72)
    }
    
    my_height_adj <- 4 - 2.1 * (650.0 / my_height)
    nf <- layout(
      mat = matrix(c(1,2,3), 3, 1, byrow = TRUE),
      #heights = c(1.9, 1.05, 1.05)
      heights = c(my_height_adj, 1.05, 1.05)
      )
    #layout.show(nf)
    norm_grow <-
      with(
        grow_df,
        cbind(
          total_ybar / total_ybar[969],
          total_min / total_min[969],
          total_mid / total_mid[969],
          total_max / total_max[969]
        )
      )
    
    my_xaxis <- function() {
      axis(
        side = 1,
        las = 2,
        labels = my_xlab,
        at = as.numeric(my_xlab)
        )
    }
    
    if (i == 1) {
      my_ylim <- c(min(norm_grow), max(norm_grow))
      my_labels <- c("0.001", "0.01", "0.1", "1", "10", "100")
      my_at <- c(0.001, 0.01, 0.1, 1, 10, 100)
    } else {
      my_ylim <- c(0.5, 1.8)
      my_labels <- c("0.2", "0.4", "0.6", "0.8", "1.0", "1.2", "1.5", "1.7")
      my_at <- as.numeric(my_labels)
    }
    old_cex <- par(cex = 0.90)
    my_mar <- par("mar")
    my_mar[1] <- 3.1
    old_mar <- par(mar = my_mar)
    matplot(
      x = grow_df$DateFraction,
      y = norm_grow,
      type = "l",
      log="y",
      lwd = my_lwd,
      lty = my_lty,
      col = my_col,
      xlim = my_xlim,
      ylim = my_ylim,
      xlab = "", # "date",
      ylab = "real growth relative to July 1961",
      axes = FALSE, #yaxp = c(.001, 100, 1),
      main = "Relative cumulative value" #, sub = "100% of proceeds reinvested"
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
      x = "topleft",
      legend = c(
        "YBAR",
        paste0(round(parm_df$Mi * 100), "% stock"),
        paste0(round(parm_df$mid * 100), "% stock"),
        paste0(round(parm_df$Ma * 100), "% stock")
        ),
      cex = my_legend_cex,
      lwd = my_lwd,
      lty = my_lty,
      col = my_col
      )
    
    my_mar[1] <- 0.1
    my_mar <- par(mar = my_mar)
    matplot(
      x = grow_df$DateFraction,
      y = grow_df$alloc,
      type = "l",
      #log="y",
      #lwd = my_lwd,
      lty = c("dashed"),
      col = c("grey50"),
      lwd = 1.5,
      xlim = my_xlim,
      xlab = "", # "date",
      ylab = "stock allocation, percent",
      axes = FALSE, #yaxp = c(.001, 100, 1),
      main = "Percentage stock allocation"
      #, sub = "100% of proceeds reinvested"
      )
    my_alloc <- grow_df$alloc
    my_alloc <- ifelse(grow_df$trade == "", NA, my_alloc)
    par(new=TRUE)
    matplot(
      x = grow_df$DateFraction,
      y = my_alloc,
      type = "l",
      #log="y",
      #lwd = my_lwd,
      lty = c("solid"),
      col = c("red"),
      xlim = my_xlim,
      xlab = "",
      ylab = "",
      # xlab = "date",
      # ylab = "stock allocation, percent",
      axes = FALSE
      #, yaxp = c(.001, 100, 1),
      #, main = "Yield-based stock allocation"
      #, sub = "100% of proceeds reinvested"
      )
    # my_xaxis()
    my_ylabels <- c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100")
    axis(
      side = 2,
      #yaxp = c(.001, 100, 1),
      las = 2,
      labels = my_ylabels,
      at = as.numeric(my_ylabels) / 100
      )
    legend(
      x = "bottomleft",
      legend = c(
        "trade to max or min",
        "resulting allocation"
        ),
      cex = my_legend_cex,
      #lwd = my_lwd,
      lty = c("solid", "dashed"),
      col = c("red", "grey50"),
      lwd = c(1, 1.5)
      )
    
    equity_premium <- grow_df$S - grow_df$B
    if (i == 1) {
      my_ylim <- c(min(equity_premium), max(equity_premium))
      my_ylabels <- c("-4", "0", "4", "8", "12", "16")
    } else {
      my_ylim <- c(-0.03, 0.05)
      my_ylabels <- c("-3", "-2", "-1", "0", "1", "2", "3", "4", "5")
    }
    matplot(
      x = grow_df$DateFraction,
      y = grow_df$S - grow_df$B,
      type = "l",
      lty = "solid",
      col = "purple",
      xlim = my_xlim,
      ylim = my_ylim,
      xlab = "",
      ylab = "difference, percent",
      axes = FALSE, #yaxp = c(.001, 100, 1),
      main = "Stock earnings yield minus nominal bond current yield"
      #, sub = "100% of proceeds reinvested"
      )
    # my_xaxis()
    axis(
      side = 2,
      #yaxp = c(.001, 100, 1),
      las = 2,
      labels = my_ylabels,
      at = as.numeric(my_ylabels) / 100
      )
    lines(my_xlim + c(-10, 10), c(0, 0), col = "black", lty = "dotted")
    # legend(
    #   x = "bottomleft",
    #   legend = c(
    #     "stock earnings yield, nominal",
    #     "bond current yield, nominal"
    #     ),
    #   cex = 0.85,
    #   #lwd = my_lwd,
    #   lty = c("dotted", "dashed"),
    #   col = c("red", "blue"),
    #   )
    
    par(old_mar)
    par(old_cex)
    dev.off()
  }
    
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
      cp_old("value_alloc_yields_1961_1981.svg", "fig5_cumulative_detail.svg")
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
      cp_img("value_alloc_yields_1961_1981.pdf", "fig5_cumulative_detail.pdf")
      cp_old("fig1_cf20yr.svg", "fig1_cf20yr_mos.svg")
      },
    ep_margin = {}
    )
}
