# invoke this file from the command line with `R --vanilla build.R`

renv::activate()

rmarkdown::render(
  input = "YieldBasedStockBondCash.Rmd"
, output_file = "YieldBasedStockBondCash.pdf"
, output_format =
    rmarkdown::pdf_document(
      dev = "pdf"
    , toc = TRUE
    , toc_depth = 2
    , number_sections = FALSE
    )
)
