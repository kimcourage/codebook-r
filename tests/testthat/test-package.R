testthat::test_that("installed resources and app do not depend on the working directory", {
  old <- setwd(tempdir())
  on.exit(setwd(old))
  testthat::expect_true(file.exists(example_path("synthetic_survey.csv")))
  testthat::expect_s3_class(create_app(), "shiny.appobj")
  data <- read_dataset(example_path("synthetic_survey.csv"))
  book <- read_codebooks(data.frame(name = "synthetic_codebook.xlsx",
                                   datapath = example_path("synthetic_codebook.xlsx")))
  rules <- heuristic_missing_rules(book$records, data)
  expected <- apply_approved_rules(data, rules)
  output_dir <- tempfile("package-export-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  archive <- file.path(output_dir, "results.zip")
  export_bundle(archive, expected, rules, variable_metadata(data), "synthetic_survey.csv")
  utils::unzip(archive, exdir = output_dir)
  reproduced <- file.path(output_dir, "reproduced.rds")
  status <- system2(file.path(R.home("bin"), "Rscript"),
    shQuote(c("--vanilla", file.path(output_dir, "preprocessing.R"),
              example_path("synthetic_survey.csv"), reproduced)), stdout = TRUE, stderr = TRUE)
  testthat::expect_null(attr(status, "status"))
  testthat::expect_equal(readRDS(reproduced), expected$data)
})
