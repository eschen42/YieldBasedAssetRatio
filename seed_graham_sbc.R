
# seed the parms (i.e. parameters) table
seed_parm <-
  (
    "
    CREATE TABLE IF NOT EXISTS
    -- table to hold parameters used in formula
    --   by default, only the row where grp = 1 is used
    parms(
      grp INT PRIMARY KEY,
      H NUM,
      Ma NUM,
      Mi NUM,
      mid NUM,
      R NUM,
      T NUM,
      W NUM,
      Y NUM,
      Z NUM,
      trgr NUM,
      first_year NUM,
      scheme_S TEXT,
      scheme_M TEXT,
      min_factor NUM, -- presented value is 2
      max_factor NUM, -- presented value is 3
      lt_lim_cutoff NUM, -- presented value 0.0375,
      lt_lim_slope NUM, -- presented value 1.7
      bond_name TEXT,
      stock_lo NUM,
      stock_hi NUM
      );
    "
  )

# insert a row of null into the parameters table, with 1 as the primary key
seed_parm_row1_insert <-
  (
    "
    -- seed the first row, which will be filled by the next statement
    --   (rather than here because because the column assignments
          --   are not easy to see or comment in an insert statement)
    INSERT INTO parms(grp) VALUES (1);
    "
  )
  
# set the bond name in the parms table
seed_parm_row1_update <-
  sprintf("UPDATE parms SET bond_name = '%s';", BOND_NAME)

# compute monthly return from bond duration
monthly_return <-
  function(., divisor = 1200, duration = BOND_DUR) {
    # divisor = 1200 to divide a percentage figure by 12 months
    #   n.b. use divisor = 12 for a fractional figure
    twelve_dur <- 1 - duration * 12 - 1
    delta <-
      function(l, r) {
        l / r + l / divisor + (1 + r / divisor) ^ twelve_dur * (1 - l / r)
      }
    sapply(
      X = 2:length(.),
      FUN = function(i) delta(.[i - 1], .[i])
    )
  }

# fetch Shiller spreadsheet and convert to SQLite DB

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

# zap rows for which earnings are not yet available
shiller_data <- shiller_data[!is.na(shiller_data$Earnings_E),]

# get 30-day T-bill data from simba spreadsheet and FRED
# ref: https://www.bogleheads.org/wiki/Simba%27s_backtesting_spreadsheet
# ref: https://www.bogleheads.org/forum/viewtopic.php?f=10&t=2520&p=3109670#p3106021 (4Nov2016)
# ref: http://www.econ.yale.edu/~shiller/data/chapt26.xlsx
tbill30_yearly_df <- read.csv("simba_tbill30.csv")
tbill30_yearly <- tbill30_yearly_df$X30_day_Tbill
names(tbill30_yearly) <- tbill30_yearly_df$Year

tbill30_monthly <-
  data.frame(
    Date_Fraction = shiller_data$Date_Fraction,
    US_30day_Tbill = 
      sapply(
        X = shiller_data$Date_Fraction,
        FUN = function(x) {
          lo <- tbill30_yearly[as.character(floor(x))]
          hi <- tbill30_yearly[as.character(ceiling(x))]
          rslt <- (x - floor(x))/(ceiling(x) - floor(x)) * (hi - lo) + lo
        }
      ) / 100
  )
tbill30 <- tbill30_monthly$US_30day_Tbill
names(tbill30) <- round(tbill30_monthly$Date_Fraction, 3)

TB4WK_df <- read.csv("TB4WK.csv")
TB4WK <- TB4WK_df$TB4WK
names(TB4WK) <- TB4WK_df$observation_date
names(TB4WK) <-
  sapply(
    X = names(TB4WK),
    FUN = function(x) {
      rslt <-
        scan(
          text = x,
          what = list(yr="", mo="", dy=""),
          sep="-",
          quiet = TRUE
          )
      round(as.numeric(rslt$yr) + (as.numeric(rslt$mo) - 0.5) / 12.0, 3)
      }
  )
TB4WK <- TB4WK[names(TB4WK) > 2001.8]
TB4WK <- TB4WK[names(TB4WK) < 0.01 + as.numeric(tail(names(tbill30), n = 1))]
# prevent division-by-zero in monthly_return()
TB4WK[TB4WK == 0] <- 0.000001

tbill30 <-
  c(
    tbill30[as.numeric(names(tbill30)) < as.numeric(head(names(TB4WK), n = 1))],
    TB4WK / 100.0
    )

gs10 <-
  shiller_data$Long_Interest_Rate_GS10 * COEF_GS10 +
  100 * COEF_SP500 / as.numeric(
    shiller_data$Cyclically_Adjusted_Price_Earnings_Ratio_P_E10_or_CAPE
  )
omit_na <- !is.na(gs10)
shiller_data <- shiller_data[omit_na, ]
gs10 <- gs10[omit_na]
tbill30 <- tbill30[omit_na]

# monthly-ize annual return; because figure imported from Simba
#   is total return, set duration to zero
Monthly_tbill30_Returns <-
  c(tbill30 |>
      monthly_return(divisor = 12, duration = 0.0), 1)


shiller_data$Monthly_Total_Bond_Returns <-
  c(gs10 |>
      monthly_return(duration = BOND_DUR), 1)

cpi <- shiller_data$Consumer_Price_Index_CPI
cpi_zplus1 <- c(cpi[2:length(cpi)], NA)
cpi_shrink <- cpi / cpi_zplus1
cpi_shrink[is.na(cpi_shrink)] <- 1

real_monthly_tbill30_returns <-
  cpi_shrink * Monthly_tbill30_Returns

real_monthly_bond_returns <-
  cpi_shrink * shiller_data$Monthly_Total_Bond_Returns

real_monthly_total_tbill30_returns <-
  Reduce(
    f = function(l, r) {
      l * r
    },
    x = real_monthly_tbill30_returns,
    init = 1,
    accumulate = TRUE
  )[1:nrow(shiller_data)]

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

shiller_data$US_Interest_Rate_30day_Tbill <- tbill30
shiller_data$Monthly_tbill30_Returns <- Monthly_tbill30_Returns
shiller_data$Real_Total_tbill30_Returns <- real_monthly_total_tbill30_returns

shiller_data <-
  shiller_data[!is.na(shiller_data[, 4]), c(1:13, 15, 17:ncol(shiller_data))]
shiller_colnames <- colnames(shiller_data)
shiller_data[, 13] <- as.numeric(shiller_data[, 13])
shiller_data[, 14] <- as.numeric(shiller_data[, 14])
shiller_data$Long_Interest_Rate_GS10 <-
  shiller_data$Long_Interest_Rate_GS10 / 100.0

recent_sh <-
  shiller_data[
    shiller_data$Date_Fraction > 1881 &
      shiller_data$Date_Fraction < RECENT_CUTOFF,
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

# shiller_data_write <-
#   sqldf::sqldf(
#     "SELECT s.*, t.US_30day_Tbill
#       FROM shiller_data s, tbill30_monthly t
#       WHERE s.Date_Fraction = t.Date_Fraction
#     "
#   )

con <- RSQLite::dbConnect(RSQLite::SQLite(), local_db)
cat("writing shiller_data\n", file = stderr())
RSQLite::dbWriteTable(con, "shiller_data", shiller_data, overwrite = TRUE)
cat("writing expected_pe\n", file = stderr())
RSQLite::dbWriteTable(con, "expected_pe", expected_pe_df, overwrite = TRUE)

RSQLite::dbExecute(con, "DROP TABLE IF EXISTS parms;")
RSQLite::dbExecute(con, seed_parm)
RSQLite::dbExecute(con, seed_parm_row1_insert)
RSQLite::dbExecute(con, seed_parm_row1_update)

RSQLite::dbDisconnect(con)
rm(con)

