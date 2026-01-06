# model VCLT as 2% plus GS10
VERUS_CASH <- FALSE
BOND_PRM <- 0.0
BOND_DUR <- 10.0

monthly_return <-
  function(., divisor = 1200, duration = BOND_DUR, premium = BOND_PRM) {
    twelve_dur <- 1 - duration * 12 - 1
    delta <-
      function(l, r) {
        l / r + l / divisor + (1 + r / divisor) ^ twelve_dur * (1 - l / r)
      }
    sapply(
      X = 2:length(.),
      FUN = function(i) delta(.[i - 1] + premium, .[i] + premium)
    )
  }
# =((G1840/G1841+G1840/1200+((1+G1841/1200)^(-119))*(1-G1840/G1841)))

# fetch Shiller spreadsheet and convert to SQLite DB

#library(readxl)

#library(RSQLite)
local_file <- "ie_data.xls"
if (!file.exists(local_file))
  tryCatch(
    download.file(
      url = "https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/02d69a38-97f2-45f8-941d-4e4c5b50dea7/ie_data.xls?ver=1743773003799",
      #url = "https://web.archive.org/web/20231223160615/http://www.econ.yale.edu/~shiller/data/ie_data.xls",
      destfile = local_file,
      method = "libcurl"
      ),
    error = function(e) {
      download.file(
        url = "http://www.econ.yale.edu/~shiller/data/ie_data.xls",
        # url = "https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/ie_data.xls?ver=1710170359099",
        # url = "https://web.archive.org/web/20240331154550/https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/ie_data.xls?ver=1710170359099",
        destfile = local_file,
        method = "libcurl"
      )
    }
  )
local_db <- "graham.sqlite"
if (TRUE || !file.exists(local_db)) {
  shiller_data <-
    readxl::read_xls(
      "ie_data.xls",
      sheet = "Data",
      skip = 8,
      col_names = FALSE
    )
  shiller_data <- as.data.frame(shiller_data)
  shiller_headers <-
    readxl::read_xls(
      "ie_data.xls",
      sheet = "Data",
      n_max = 7,
      col_names = FALSE
    )
  shiller_headers <- as.data.frame(shiller_headers)
  shiller_colnames <-
    sapply(
      X = seq_along(shiller_headers),
      FUN = function(i) {
        shiller_headers[,i] |>
          (function(.) .[!is.na(.)])() |>
          paste(collapse="_") |>
          (function(.) gsub(" ","_",.))() |>
          (function(.) gsub("[&/.]","_",.))()
        }
    )
  shiller_colnames[1] <- "Date"
  colnames(shiller_data) <- shiller_colnames
  shiller_data$Monthly_Total_Bond_Returns <-
    c(shiller_data$Long_Interest_Rate_GS10 |>
        monthly_return(duration = BOND_DUR, premium = BOND_PRM),
      1
      )
  cpi <- shiller_data$Consumer_Price_Index_CPI
  cpi_zplus1 <- c(cpi[2:length(cpi)], NA)
  cpi_shrink <- cpi / cpi_zplus1
  cpi_shrink[is.na(cpi_shrink)] <- 1
  real_monthly_bond_returns <-
    cpi_shrink * shiller_data$Monthly_Total_Bond_Returns
  
  real_monthly_total_bond_returns <-
    Reduce(
      f = function(l, r) {
        l * r
      },
      x = real_monthly_bond_returns,
      init = 1,
      accumulate = TRUE
    )[1:nrow(shiller_data)]
  
  shiller_data$Real_Total_Bond_Returns <- real_monthly_total_bond_returns
  shiller_data <- shiller_data[!is.na(shiller_data[, 4]), c(1:13, 15, 17:22)]
  shiller_colnames <- colnames(shiller_data)
  shiller_data[, 13] <- as.numeric(shiller_data[, 13])
  shiller_data[, 14] <- as.numeric(shiller_data[, 14])
  shiller_data$Long_Interest_Rate_GS10 <-
    shiller_data$Long_Interest_Rate_GS10 / 100.0
  if (VERUS_CASH) {
    shiller_data$Long_Interest_Rate_GS10 <- 0.05
  }
  # =((G1840/G1841+G1840/1200+((1+G1841/1200)^(-119))*(1-G1840/G1841))) 
  recent_sh <-
    shiller_data[
      shiller_data$Date_Fraction > 1881 & shiller_data$Date_Fraction < 2023,
      ]
  
  Xpe10 <-
    X <-
    line(
      x = recent_sh$Date_Fraction,
      y = recent_sh$Cyclically_Adjusted_Price_Earnings_Ratio_P_E10_or_CAPE,
      iter = 5
      )
  shiller_data$TukeyPE10 <-
    Xpe10$coefficients[1] + shiller_data$Date_Fraction * Xpe10$coefficients[2]
  with(
    recent_sh,
    {
      plot(
        x = Date_Fraction,
        y = Cyclically_Adjusted_Price_Earnings_Ratio_P_E10_or_CAPE,
        ylab = "P/E10 or CAPE",
        xlab = "year",
        main = "Tukey line fitted to P/E10",
        sub = paste(
          "expected P/E10 = ",
          signif(Xpe10$coefficients[1], 8),
          "+ (year)(",
          signif(Xpe10$coefficients[2], 8),
          ")"),
        type = "l"
        )
      lines(
        x = Date_Fraction[!is.na(Cyclically_Adjusted_Price_Earnings_Ratio_P_E10_or_CAPE)],
        y = Xpe10$fitted.values,
        col = "red"
        )
      }
    )
  expected_pe_df <-
    data.frame(
      trend = "P/E10",
      intercept = signif(Xpe10$coefficients[1], 8),
      annual_growth = signif(Xpe10$coefficients[2], 8)
    )
  
  Xpe <-
    X <-
    line(
      x = recent_sh$Date_Fraction,
      y = recent_sh$S_P_Comp__P / recent_sh$Earnings_E,
      iter = 5
      )
  shiller_data$TukeyPE <-
    Xpe$coefficients[1] + shiller_data$Date_Fraction * Xpe$coefficients[2]
  with(
    recent_sh,
    {
      plot(
        x = Date_Fraction,
        y = recent_sh$S_P_Comp__P / recent_sh$Earnings_E,
        ylab = "P/E",
        xlab = "year",
        main = "Tukey line fitted to P/E",
        sub = paste(
          "expected P/E = ",
          signif(Xpe$coefficients[1], 8),
          "+ (year)(",
          signif(Xpe$coefficients[2], 8),
          ")"),
        type = "l"
        )
      lines(
        x = Date_Fraction[!is.na(Cyclically_Adjusted_Price_Earnings_Ratio_P_E10_or_CAPE)],
        y = Xpe$fitted.values,
        col = "red"
        )
      }
    )
  expected_pe_df <-
    rbind(
      expected_pe_df,
      data.frame(
        trend = "P/E",
        intercept = signif(Xpe$coefficients[1], 8),
        annual_growth = signif(Xpe$coefficients[2], 8)
      )
    )
  
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "graham.sqlite")
  cat("writing shiller_data", file = stderr())
  RSQLite::dbWriteTable(con, "shiller_data", shiller_data, overwrite = TRUE)
  cat("writing expected_pe", file = stderr())
  RSQLite::dbWriteTable(con, "expected_pe", expected_pe_df, overwrite = TRUE)
  
  RSQLite::dbDisconnect(con)
  rm(con)
}
