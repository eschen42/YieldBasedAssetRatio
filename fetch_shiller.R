# fetch Shiller spreadsheet and convert to SQLite DB
#library(readxl)
#library(RSQLite)
local_db <- "graham.sqlite"
local_file <- "ie_data.xls"
if (!file.exists(local_db)) {
  if (!file.exists(local_file)) download.file(
    url = "https://web.archive.org/web/20231223160615/http://www.econ.yale.edu/~shiller/data/ie_data.xls",
    destfile = local_file,
    method = "libcurl"
    )
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
  shiller_data <- shiller_data[!is.na(shiller_data[, 4]), c(1:13, 15, 17:22)]
  shiller_colnames <- colnames(shiller_data)
  shiller_data[, 13] <- as.numeric(shiller_data[, 13])
  shiller_data[, 14] <- as.numeric(shiller_data[, 14])
  shiller_data$Long_Interest_Rate_GS10 <-
    shiller_data$Long_Interest_Rate_GS10 / 100.0
  
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
  RSQLite::dbWriteTable(con, "shiller_data", shiller_data, overwrite = TRUE)
  RSQLite::dbWriteTable(con, "expected_pe", expected_pe_df, overwrite = TRUE)
  
  RSQLite::dbDisconnect(con)
  rm(con)
}
