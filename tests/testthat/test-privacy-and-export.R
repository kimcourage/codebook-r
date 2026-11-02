testthat::test_that("AI preview does not include respondent-level values", {
  d <- data.frame(variable = c("SECRET_PERSON_A", "SECRET_PERSON_B"),
                  q = c(1L, 9L), stringsAsFactors = FALSE)
  cb <- list(records = empty_codebook_records(), excerpts = c("test" = "Codebook only"))
  x <- make_ai_request_preview(d, cb)
  testthat::expect_false(grepl("SECRET_PERSON_A", x, fixed = TRUE))
  testthat::expect_true(grepl('"variable"', x, fixed = TRUE))
})

testthat::test_that("standalone script contains approved rules, no API keys", {
  r <- empty_rules()
  r <- rbind(r, data.frame(id = 1L, variable = "gender", operation = "set_na",
          values = "9", minimum = NA_real_, maximum = NA_real_, reason = "test",
          evidence = "book", confidence = "high", source = "manual"))
  repo_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  script <- paste(make_standalone_script(r, root = repo_root), collapse = "\n")
  testthat::expect_true(grepl("apply_approved_rules", script, fixed = TRUE))
  testthat::expect_true(grepl("approved_rules <-", script, fixed = TRUE))
  testthat::expect_false(grepl("OPENAI_API_KEY", script, fixed = TRUE))
})

testthat::test_that("bundle can be created and contains data, script, and audit", {
  d <- data.frame(gender = c(1L, 9L, 2L))
  r <- data.frame(id = 1L, variable = "gender", operation = "set_na",
                  values = "9", minimum = NA_real_, maximum = NA_real_,
                  reason = "missing", evidence = "S!row 3", confidence = "medium",
                  source = "test")
  res <- apply_approved_rules(d, r)
  p <- tempfile(fileext = ".zip")
  repo_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  export_bundle(p, res, r, variable_metadata(d), "survey.csv", root = repo_root)
  testthat::expect_true(file.exists(p))
  listing <- utils::unzip(p, list = TRUE)$Name
  testthat::expect_true(all(c("cleaned_data.rds", "preprocessing.R", "audit.csv") %in% listing))
})

testthat::test_that("standalone script reproduces explicit CSV import options and Korean data", {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  dir <- tempfile("reproduce-"); dir.create(dir)
  input <- file.path(dir, "original.csv")
  writeBin(charToRaw(iconv("id,question,note\n123,1,동의\n456,9,무응답\n", from = "UTF-8", to = "CP949")), input)
  options <- list(csv_encoding = "CP949", character_columns = "id")
  d <- do.call(read_dataset, c(list(path = input), options))
  r <- data.frame(id = 1L, variable = "question", operation = "set_na", values = "9",
    minimum = NA_real_, maximum = NA_real_, reason = "Synthetic missing code", evidence = "Synthetic book",
    confidence = "manual", source = "manual")
  expected <- apply_approved_rules(d, r)
  script <- file.path(dir, "preprocessing.R")
  writeLines(make_standalone_script(r, root = root, import_options = options), script, useBytes = TRUE)
  output <- file.path(dir, "result.rds")
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(c("--vanilla", script, input, output)), stdout = TRUE, stderr = TRUE)
  testthat::expect_null(attr(status, "status"))
  actual <- readRDS(output)
  testthat::expect_identical(actual, expected$data)
  testthat::expect_identical(actual$id, c("123", "456"))
  testthat::expect_identical(actual$note, c("동의", "무응답"))
})
