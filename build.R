# invoke this file from the command line with `R --vanilla build.R`

renv::activate()

rmarkdown::render(
  input = "YieldBasedAssetRatio.Rmd"
, output_file = "YieldBasedAssetRatio.pdf"
, output_format =
    rmarkdown::pdf_document(
      dev = "pdf"
    , toc = TRUE
    , toc_depth = 3
    , number_sections = FALSE
    )
)
