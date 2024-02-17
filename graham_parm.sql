-- computational core for the Yield-Based Asset Ratio described in
--   https://eschenlauer.com/investing/risk_based_allocation/YBAR_intro.html

CREATE TABLE IF NOT EXISTS
  -- table derived from Robert J. Shiller's spreadsheet:
  --   https://web.archive.org/web/20231223160615/http://www.econ.yale.edu/~shiller/data/ie_data.xls
  `shiller_data` (
    `Date` REAL,
    `S_P_Comp__P` REAL,
    `Dividend_D` REAL,
    `Earnings_E` REAL,
    `Consumer_Price_Index_CPI` REAL,
    `Date_Fraction` REAL PRIMARY KEY,
    -- Long_Interest_Rate_GS10 here is 100 divided into Shiller's percentage-based column
    `Long_Interest_Rate_GS10` REAL,
    `Real_Price` REAL,
    `Real_Dividend` REAL,
    `Real_Total_Return_Price` REAL,
    `Real_Earnings` REAL,
    `Real_TR_Scaled_Earnings` REAL,
    `Cyclically_Adjusted_Price_Earnings_Ratio_P_E10_or_CAPE` REAL,
    `Cyclically_Adjusted_Total_Return_Price_Earnings_Ratio_TR_P_E10_or_TR_CAPE` REAL,
    `Excess_CAPE_Yield` REAL,
    `Monthly_Total_Bond_Returns` REAL,
    `Real_Total_Bond_Returns` REAL,
    `10_Year_Annualized_Stock_Real_Return` REAL,
    `10_Year_Annualized_Bonds_Real_Return` REAL,
    `Real_10_Year_Excess_Annualized_Returns` REAL,
    -- Tukey fit for expected P/E10 - R: line(x, y, iter=5)
    `TukeyPE10` REAL,
    -- Tukey fit for expected P/E - R: line(x, y, iter=5)
    `TukeyPE` REAL
  );

.print '---'
.print 'Dropping tables, views, and indexes created in this script'
DROP INDEX IF EXISTS shiller_data__DateFraction__idx;
DROP VIEW  IF EXISTS growth_months;
DROP TABLE IF EXISTS parms;
DROP VIEW  IF EXISTS parm;
DROP VIEW  IF EXISTS graham_base;
DROP VIEW  IF EXISTS graham_gain;
DROP VIEW  IF EXISTS alloc_cap;
--DROP VIEW  IF EXISTS alloc;
DROP VIEW  IF EXISTS guide;
DROP VIEW  IF EXISTS grow;
DROP VIEW  IF EXISTS growth;
DROP VIEW  IF EXISTS growth_min;
DROP VIEW  IF EXISTS relative_growth_min;
DROP VIEW  IF EXISTS growth_max;
DROP VIEW  IF EXISTS relative_growth_max;
DROP VIEW  IF EXISTS growth_min2max;
DROP VIEW  IF EXISTS relative_growth_min2max;

.print ' '
.print 'Remaining tables, views, and indexes:'
.tables
.print ' '
.print 'Creating tables, views, and indexes'
CREATE INDEX IF NOT EXISTS
  shiller_data__DateFraction__idx ON shiller_data(Date_Fraction);

CREATE VIEW IF NOT EXISTS
  -- express 1-50 years as months
  growth_months
  AS
  WITH RECURSIVE
    cnt(x) AS (
      VALUES(1)
      UNION ALL
      SELECT x + 1 FROM cnt WHERE x < 50
      )
  SELECT
    x AS years,
    12 * x AS months
  FROM cnt
  LIMIT 10000
  ;

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
    scheme_M TEXT
  );

-- seed the first row, which will be filled by the next statement
--   (rather than here because because the column assignments
--   are not easy to see or comment in an insert statement)
INSERT INTO parms(grp) VALUES (1), (2);

-- see https://eschenlauer.com/investing/risk_based_allocation/YBAR_intro.html
--   for guidance regarding the parameters chosen here
UPDATE parms
  SET H    = 0.25,   -- maximum tolerable "loss on paper" when past margin of reversion
      Ma   = 0.88,   -- maximum acceptable stock proportion in portfolio
      Mi   = 0.06,   -- minimum acceptable stock proportion in portfolio
      mid  = 0.60,   -- mid-way allocation (avg YBAR for 1911-2022 with [Mi,Ma] = [0.06,0.88])
      R    = 0.012,  -- offset minimum stock pct; lowering decreases capital appreciation
      T    = 0.0414, -- historic bond CAGR
      W    = 0.0645, -- historic stock CAGR
      Y    = 0.0400, -- the mean of W and the historic T-bill CAGR (1.54%)
      Z    = 0.0303, -- 1 / (95th percentile for P/E10)
      trgr = 0.06,   -- amount beyond limit when is trade triggered
      first_year = 1897,   -- first year for rolling period calculations
      scheme_S = 'e10p', -- 'e10p' use E10/P for stock earnings yield (S)
      -- scheme_S = 'ep', -- 'ep' use E/P for stock earnings yield (S)
      -- scheme_M = 'RTWYZ' -- 'RTWYZ' use R, T, W, Y, Z for threshold-setting
      scheme_M = 'margin' -- 'margin" use margins of safety and folly for threshold-setting
  WHERE grp = 2
  ;

UPDATE parms
  SET H    = 0.25,   -- maximum tolerable 'loss on paper' when past margin of reversion
      Ma   = 0.87,   -- maximum acceptable stock proportion in portfolio
      --Ma   = 1.00,   -- maximum acceptable stock proportion in portfolio
      Mi   = 0.03,   -- minimum acceptable stock proportion in portfolio
      --Mi   = 0.00,   -- minimum acceptable stock proportion in portfolio
      mid  = 0.60,   -- mid-way allocation (avg YBAR for 1911-2022 with [Mi,Ma] = [0.06,0.88])
      --mid  = 0.6477,   -- mid-way allocation (avg YBAR for 1911-2022 with [Mi,Ma] = [0.06,0.88])
      R    = 0.012,  -- offset minimum stock pct; lowering decreases capital appreciation
      T    = 0.0414, -- historic bond CAGR
      W    = 0.0645, -- historic stock CAGR
      Y    = 0.0400, -- the mean of W and the historic T-bill CAGR (1.54%)
      Z    = 0.0303, -- 1 / (95th percentile for P/E10)
      trgr = 0.03,   -- amount beyond limit when is trade triggered
      --trgr = 0.00,   -- amount beyond limit when is trade triggered
      first_year = 1911,   -- first year for rolling period calculations
      scheme_S = 'e10p', -- 'e10p' use E10/P for stock earnings yield (S)
      -- scheme_S = 'ep', -- 'ep' use E/P for stock earnings yield (S)
      scheme_M = '' -- '' use R, T, W, X, Y, Z for threshold-setting
      -- scheme_M = 'margin' -- 'margin' use margins of safety and folly for threshold-setting
  WHERE grp = 1
  ;

CREATE VIEW IF NOT EXISTS
  -- this view is used to supply parameters to queries;
  --   redefine the view if you require a different row from parms
  parm
  AS SELECT * FROM parms WHERE grp = 1
  ;

CREATE VIEW IF NOT EXISTS
  -- adjust shiller_data for usability in the graham_gain view
  graham_base
  AS
  SELECT
    Date_Fraction,
    `S_P_Comp__P`,
    `Dividend_D`,
    `Earnings_E`,
    `Long_Interest_Rate_GS10`,
    `Consumer_Price_Index_CPI`,
    `Cyclically_Adjusted_Price_Earnings_Ratio_P_E10_or_CAPE`
      AS pe10,
    `Real_Total_Return_Price`,
    `Real_Total_Bond_Returns`,
    -- John Tukey's "median-median line"
    --   (https://en.wikipedia.org/wiki/Median#Median%E2%80%93median_line)
    --   is the estimator for the expected P/E10 rather than a least
    --   squares line because distribution of the P/E10 deviates
    --   considerably from normality. Approximately:
    --     Date_Fraction * 0.0578777 - 95.694931 AS expected_pe10,
    `TukeyPE10`
      AS expected_pe10,
    `Cyclically_Adjusted_Price_Earnings_Ratio_P_E10_or_CAPE` / `TukeyPE10`
      AS relative_pe10,
    `TukeyPE`
      AS expected_pe,
    (`S_P_Comp__P` / `Earnings_E`) / `TukeyPE`
      AS relative_pe,
    ROWID
  FROM shiller_data
  WHERE pe10 IS NOT NULL
  ;

CREATE VIEW IF NOT EXISTS
  -- adjust shiller_data for usability in the YBAR formula
  graham_gain
  AS
  SELECT
    -- year (as integer) and month (as fraction)
    t1.Date_Fraction              AS DateFraction,
    -- use E10/P as the earnings yield for stocks
    CASE p.scheme_S
      WHEN 'e10p' THEN
        1.0 / t1.pe10
      ELSE
        t1.`Earnings_E` / t1.`S_P_Comp__P`
      END                         AS S,
    -- use the current yield of 10-year US Treasury bonds
    t1.Long_Interest_Rate_GS10    AS B,
    -- although unused, the CPI could be nice as a reference
    t1.Consumer_Price_Index_CPI   AS cpi,
    -- compute monthly increase in real total returns for stock
    t2.Real_Total_Return_Price /
      t1.Real_Total_Return_Price  AS S_real_grow,
    -- compute monthly increase in real total returns for bonds
    t2.Real_Total_Bond_Returns /
      t1.Real_Total_Bond_Returns  AS B_real_grow,
    -- P/E10 is last used in the where clause of this query
    -- t1.pe10                       AS pe10,
    -- t1.relative_pe10              AS relative_pe10,
    -- historically expected P/E10 for calculating allocation cap
    CASE p.scheme_S
      WHEN 'e10p' THEN
        t1.expected_pe10
      ELSE
        t1.expected_pe
      END                         AS X
  FROM
    graham_base t1,
    graham_base t2,
    parm p
  WHERE t1.pe10 IS NOT NULL
    AND t1.ROWID + 1 = t2.ROWID
    ;

-- allocation cap
--   C(S, H, X) = H / max(H, 1.0 / (S * X) - 1)
--     Note that relative-PE10 == 1/(S*X)
CREATE VIEW IF NOT EXISTS
  -- From YBAR_intro.html, op cit:
  --   allocation cap
  --     C(S, H, X) = H / max(H, 1 / (S * X) - 1)
  -- Note that relative-PE10 == 1 /(S * X)
  alloc_cap
  AS
  SELECT
    a.*,
    H / max(H, 1.0 / (S * X) - 1) AS C
  FROM (
    SELECT
      DateFraction,
      S,
      B,
      X,
      S_real_grow,
      B_real_grow
    FROM graham_gain gb
  ) a, parm p
  ;

CREATE VIEW IF NOT EXISTS
  -- From YBAR_intro.html, op cit:
  --   allocation minimum
  --     min_stock_percentage = min(C, Ma, max(0,  (S - B - 0.0009)/0.0222))
  --   allocation maximum
  --     max_stock_percentage = min(C, Ma, max(Mi, (S - B + 0.0111)/0.0097))
  guide
  AS
  SELECT
    -- add a sequence number for use in joins
    row_number() OVER (ORDER BY DateFraction)
                     AS seq,
    DateFraction,
    X,
    H,
    C,
    S,
    B,
    S_real_grow,
    B_real_grow,
    -- threshold that triggers stock purchase
    min_stock - trgr AS min_trigger,
    min_stock,
    max_stock,
    -- threshold that triggers stock sale
    max_stock + trgr AS max_trigger,
    0 AS total_bond,
    1 AS total_stock
  FROM
    ( SELECT
        a.*,
        -- from YBAR_intro.html, op cit:
        -- allocation minimum =
        --   min(C, Ma, max(0,  (S - B - 0.0009)/0.0222))
        CASE p.scheme_M
          WHEN 'margin' THEN
            min(C, Ma, max(0, (2 * B / T) * (S / B - 1)))
          ELSE
            min(C, Ma, max(0,  (S - B + T - Z - R) / (W - Z - R)))
          END
          AS min_stock,
        -- allocation maximum =
        --   min(C, Ma, max(Mi, (S - B + 0.0111)/0.0097))
        CASE p.scheme_M
          WHEN 'margin' THEN
            min(C, Ma, max(Mi, 1 - (3 * B / T) * (B / S - 1)))
          ELSE
            min(C, Ma, max(Mi, (S - B + T - Z    ) / (Y - Z    )))
          END
          AS max_stock
      FROM
        alloc_cap a, parm p
    ) a,
    parm p
  ;

CREATE VIEW IF NOT EXISTS
  -- apply YBAR formula for each month:
  --   - if stock allocation < min_trigger, buy stock till allocation = min_stock
  --   - if stock allocation > max_trigger, sell stock till allocation = max_stock
  --   - otherwise, do not change allocation
  -- compute monthly gains for:
  --   - YBAR
  --   - minimum fixed percentage allocation (parm: Mi)
  --   - middle fixed percentage allocation (parm: mid)
  --   - maximum fixed percentage allocation (parm: Ma)
  grow
  AS
  WITH RECURSIVE
    grw(
      seq,
      DateFraction,
      X,
      H,
      C,
      S,
      B,
      S_real_grow,
      B_real_grow,
      min_trigger,
      min_stock,
      max_stock,
      max_trigger,
      total_bond,
      total_stock,
      stock_val,
      bond_val,
      total_ybar,
      prev_alloc,
      alloc,
      trade,
      total_min,
      total_mid,
      total_max
      )
    AS (
      SELECT
        a.seq,
        a.DateFraction,
        a.X,
        a.H,
        a.C,
        a.S,
        a.B,
        a.S_real_grow,
        a.B_real_grow,
        a.min_trigger,
        a.min_stock,
        a.max_stock,
        a.max_trigger,
        1 AS total_bond,
        1 AS total_stock,
        --a.*,
        a.max_stock AS stock_val,
        1 - a.max_stock AS bond_val,
        1 AS total_ybar,
        a.max_stock AS prev_alloc,
        a.max_stock AS alloc,
        '.' AS trade,
        1 AS total_min,
        1 AS total_mid,
        1 AS total_max
      FROM guide a
      WHERE a.seq = 1
      UNION ALL
      SELECT
        a.seq,
        a.DateFraction,
        a.X,
        a.H,
        a.C,
        a.S,
        a.B,
        a.S_real_grow,
        a.B_real_grow,
        a.min_trigger,
        a.min_stock,
        a.max_stock,
        a.max_trigger,
        -- compute monthly gains for all-bond allocation
        g.total_bond * g.B_real_grow
          AS total_bond,
        -- compute monthly gains for all-stock allocation
        g.total_stock * g.S_real_grow
          AS total_stock,
        -- compute monthly gains for YBAR
        g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow
          AS stock_val,
        (1 - g.alloc) * (g.stock_val + g.bond_val) * g.B_real_grow
          AS bond_val,
        -- stock_val + bond_val
        g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow
          + (1 - g.alloc) * (g.stock_val + g.bond_val) * g.B_real_grow
          AS total_ybar,
        g.stock_val / (g.stock_val + g.bond_val) AS prev_alloc,
        -- apply YBAR formula for each month:
        --   - if stock allocation < min_trigger, buy stock till allocation = min_stock
        --   - if stock allocation > max_trigger, sell stock till allocation = max_stock
        --   - otherwise, do not change allocation
        CASE
          WHEN
            (g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow) / (
              (g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow) +
                ((1 - g.alloc) * (g.stock_val + g.bond_val) * g.B_real_grow)
            ) < g.min_trigger AND g.alloc < g.min_stock
            THEN g.min_stock
          WHEN
            (g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow) / (
              (g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow) +
                ((1 - g.alloc) * (g.stock_val + g.bond_val) * g.B_real_grow)
            ) > g.max_trigger AND g.alloc > g.max_stock
            THEN g.max_stock
          ELSE
            (g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow) / (
              (g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow) +
                ((1 - g.alloc) * (g.stock_val + g.bond_val) * g.B_real_grow)
            )
          END
          AS alloc,
        CASE
          WHEN
            (g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow) / (
              (g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow) +
                ((1 - g.alloc) * (g.stock_val + g.bond_val) * g.B_real_grow)
            ) < g.min_trigger AND g.alloc < g.min_stock
            THEN '+'
          WHEN
            (g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow) / (
              (g.alloc * (g.stock_val + g.bond_val) * g.S_real_grow) +
                ((1 - g.alloc) * (g.stock_val + g.bond_val) * g.B_real_grow)
            ) > g.max_trigger AND g.alloc > g.max_stock
            THEN '-'
          ELSE ''
          END
          AS trade,
        -- compute monthly gains for minimum fixed percentage allocation (parm: Mi)
        (1 - p.Mi) * g.total_min * g.B_real_grow
          + p.Mi * g.total_min * g.S_real_grow
          AS total_min,
        -- compute monthly gains for middle fixed percentage allocation (parm: mid)
        (1 - p.mid) * g.total_mid * g.B_real_grow
          + p.mid * g.total_mid * g.S_real_grow
          AS total_mid,
        -- compute monthly gains for  maximum fixed percentage allocation (parm: Ma)
        (1 - p.Ma) * g.total_max * g.B_real_grow
          + p.Ma * g.total_max * g.S_real_grow
          AS total_max
      FROM 
        guide a,
        parm p,
        grw g
      WHERE a.seq = g.seq + 1
      LIMIT 1000000
    )
  SELECT * FROM grw
  ;

CREATE VIEW IF NOT EXISTS
  -- compute proportionate growth over intervals specified in growth_months
  growth
  AS
  SELECT
    g.years,
    g.months,
    t_start.DateFraction                    AS date_from,
    t_end.DateFraction                      AS date_thru,
    t_end.total_ybar  / t_start.total_ybar  AS grow_ybar,
    t_end.total_min   / t_start.total_min   AS grow_min,
    t_end.total_mid   / t_start.total_mid   AS grow_mid,
    t_end.total_max   / t_start.total_max   AS grow_max,
    t_end.total_bond  / t_start.total_bond  AS grow_bond,
    t_end.total_stock / t_start.total_stock AS grow_stock,
    t_start.B,
    t_start.S
  FROM
    grow t_end, grow t_start, growth_months g, parm
  WHERE t_end.seq = t_start.seq + g.months
    AND t_start.DateFraction > parm.first_year
  ;

CREATE VIEW IF NOT EXISTS
  -- compute minimum proportionate growth, by interval length
  growth_min
  AS
  SELECT
    years,
    min(grow_ybar) AS min_grow_ybar,
    min(grow_min)  AS min_grow_min,
    min(grow_mid)  AS min_grow_mid,
    min(grow_max)  AS min_grow_max
  FROM growth
  GROUP BY years
  ;

CREATE VIEW IF NOT EXISTS
  -- compute minimum proportionate growth relative to YBAR, by interval length
  relative_growth_min
  AS
  SELECT
    years,
    min_grow_min / min_grow_ybar,
    min_grow_mid / min_grow_ybar,
    min_grow_max / min_grow_ybar
  FROM growth_min
  ;

CREATE VIEW IF NOT EXISTS
  -- compute maximum proportionate growth, by interval length
  growth_max
  AS
  SELECT
    years,
    max(grow_ybar) AS max_grow_ybar,
    max(grow_min)  AS max_grow_min,
    max(grow_mid)  AS max_grow_mid,
    max(grow_max)  AS max_grow_max
  FROM growth
  GROUP BY years
  ;

CREATE VIEW IF NOT EXISTS
  -- compute maximum proportionate growth relative to YBAR, by interval length
  relative_growth_max
  AS
  SELECT
    years,
    max_grow_min / max_grow_ybar,
    max_grow_mid / max_grow_ybar,
    max_grow_max / max_grow_ybar
  FROM growth_max
  ;

CREATE VIEW IF NOT EXISTS
  -- compute spread in proportionate growth, by interval length
  growth_min2max
  AS
  SELECT
    Ma.years,
    Ma.max_grow_ybar - Mi.min_grow_ybar AS spread_ybar,
    Ma.max_grow_min  - Mi.min_grow_min  AS spread_min,
    Ma.max_grow_mid  - Mi.min_grow_mid  AS spread_mid,
    Ma.max_grow_max  - Mi.min_grow_max  AS spread_max
  FROM growth_max Ma, growth_min Mi
  WHERE Ma.years = Mi.years
  ;

CREATE VIEW IF NOT EXISTS
  -- compute spread in proportionate growth relative to YBAR, by interval length
  relative_growth_min2max
  AS
  SELECT
    years,
    spread_min / spread_ybar,
    spread_mid / spread_ybar,
    spread_max / spread_ybar
  FROM growth_min2max
  ;

.print ' '
.print 'Final tables and views:'
.tables
.print '...'
